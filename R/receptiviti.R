.onLoad <- function(lib, pkg) {
  if (Sys.getenv("RECEPTIVITI_URL") == "") Sys.setenv(RECEPTIVITI_URL = "https://api.receptiviti.com/")
}

#' Receptiviti API
#'
#' The main function to access the \href{https://www.receptiviti.com}{Receptiviti} API.
#'
#' @param text A character vector with text to be processed, path to a directory containing files, or a vector of file paths.
#' @param output Path to a \code{.csv} file to write results to. If this already exists, it will be loaded instead of
#' processing any text.
#' @param text_column Column name of text, if \code{text} is a matrix-like object, or a path to a csv file.
#' @param file_ext File extension to search for, if \code{text} is the path to a directory containing files to be read in.
#' @param retry_limit Maximum number of times each request can be retried after hitting a rate limit.
#' @param cache Logical; if \code{FALSE}, will not temporarily store responses, to avoid making the same requests multiple times.
#' @param clear_cache Logical; if \code{TRUE}, will clear any existing files in the cache.
#' @param verbose Logical; if \code{TRUE}, will show status messages.
#' @returns A \code{data.frame} with columns for \code{text} (the originally entered text), \code{response_id}, \code{language},
#' and \code{version} (metadata from the API), and scores from each included framework (e.g., \code{summary.word_count} and \code{liwc.i}).
#' @param key API Key; defaults to \code{Sys.getenv("RECEPTIVITI_KEY")}.
#' @param secret API Secret; defaults to \code{Sys.getenv("RECEPTIVITI_SECRET")}.
#' @param url API endpoint; defaults to \code{Sys.getenv("RECEPTIVITI_URL")}, which defaults to
#' \code{"https://api.receptiviti.com/"}.
#' @examples
#' \dontrun{
#' # score a single text
#' single <- receptiviti("a text to score")
#'
#' # score multiple texts, and write results to a file
#' multi <- receptiviti(c("first text to score", "second text"), "filename.csv")
#' }
#' @importFrom curl new_handle curl_fetch_memory new_pool handle_setheaders handle_setopt
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom utils object.size read.csv write.csv
#' @importFrom digest digest
#' @importFrom parallel detectCores makeCluster clusterExport parLapplyLB stopCluster
#' @importFrom arrow read_csv_arrow write_csv_arrow
#' @export

receptiviti <- function(text, output = NULL, text_column = NULL, file_ext = "txt", retry_limit = 10,
                        cache = TRUE, clear_cache = FALSE, verbose = FALSE,
                        key = Sys.getenv("RECEPTIVITI_KEY"), secret = Sys.getenv("RECEPTIVITI_SECRET"),
                        url = Sys.getenv("RECEPTIVITI_URL")) {
  # check input
  if (!is.null(output)) {
    output <- paste0(sub("\\.csv.*$", "", output), ".csv")
    if (file.exists(output)) {
      if (verbose) message("reading in existing output")
      return(as.data.frame(read_csv_arrow(output)))
    }
  }
  if (missing(text)) stop("enter text as the first argument", call. = FALSE)
  if (is.character(text) && length(text) == 1 && nchar(text) < 1000 && dir.exists(text)) {
    text <- list.files(text, file_ext)
  }
  if (!is.character(text)) {
    if (text_column %in% colnames(text)) text <- text[, text_column, drop = TRUE]
    if (length(text) && !is.character(text)) text <- as.character(text)
    if (!is.character(text)) stop("text must be a character vector, or such a column identified by text_column", call. = FALSE)
  }
  if (key == "") stop("specify your key, or set it to the RECEPTIVITI_KEY environment variable", call. = FALSE)
  if (secret == "") stop("specify your secret, or set it to the RECEPTIVITI_SECRET environment variable", call. = FALSE)
  if (!is.numeric(retry_limit)) retry_limit <- 0
  url <- paste0(sub("(?:/v\\d+)?/+$", "", url), "/v1/")

  # ping API
  handler <- curl::new_handle(httpauth = 1, userpwd = paste0(key, ":", secret))
  ping <- curl::curl_fetch_memory(paste0(url, "ping"), handler)
  if (ping$status_code != 200) {
    res <- list(message = rawToChar(ping$content))
    if (substring(res$message, 1, 1) == "{") res <- jsonlite::fromJSON(res$message)
    stop(paste0(if (length(res$code)) paste0(ping$status_code, " (", res$code, "): "), res$message), call. = FALSE)
  }

  # prepare text
  bundles <- list()
  bundle_index <- 1
  index <- 0
  size <- 0
  length <- 0
  for (t in text) {
    if (!is.na(t) && t != "") {
      entry <- list(content = t)
      size <- size + object.size(entry) + 200
      length <- length + 1
      if (length > 1e3 || size > 1e7) {
        bundle_index <- bundle_index + 1
        index <- 1
        length <- 0
        size <- 0
      } else {
        index <- index + 1
      }
      if (length(bundles) < bundle_index) bundles[[bundle_index]] <- list()
      bundles[[bundle_index]][[index]] <- entry
    }
  }

  if (cache) {
    temp <- paste0(dirname(tempdir()), "/receptiviti/")
    dir.create(temp, FALSE)
    if (clear_cache) unlink(list.files(temp, full.names = TRUE))
  }

  # make request(s)
  endpoint <- paste0(url, "framework/bulk")
  retrieved <- 0
  results <- list()
  pool <- curl::new_pool()
  done <- NULL
  failure <- function(message) stop("request failed: ", message)
  unpack <- function(d) {
    if (is.list(d)) as.data.frame(lapply(d, unpack), optional = TRUE) else d
  }
  if (verbose) cat("\r", retrieved, "of", length(bundles), "bundles received")
  for (i in seq_along(bundles)) {
    if (cache) {
      bundle_file <- paste0(temp, digest::digest(bundles[[i]]), ".csv")
      if (file.exists(bundle_file)) {
        retrieved <- retrieved + 1
        if (verbose) cat("\r", retrieved, "of", length(bundles), "bundles received (cached)")
        results[[i]] <- read_csv_arrow(bundle_file)
        next
      }
    }
    env <- new.env()
    env$retry_limit <- retry_limit
    env$index <- i
    if (cache) env$bundle_file <- bundle_file
    exclude_cols <- c("response_id", "language", "version", "error", "custom")
    env$done <- function(res) {
      if (res$status_code == 200) {
        result <- jsonlite::fromJSON(rawToChar(res$content))$results
        if ("error" %in% names(result)) {
          su <- !is.na(result$error$code)
          errors <- result[su & !duplicated(result$error$code), "error"]
          warning(
            if (sum(su) > 1) "some texts were invalid: " else "a text was invalid: ",
            paste(do.call(paste0, data.frame("(", errors$code, ") ", errors$message)), collapse = "; "),
            call. = FALSE
          )
        }
        results[[index]] <<- cbind(
          text = vapply(bundles[[index]], "[[", "", "content"),
          unpack(result[, !names(result) %in% exclude_cols])
        )
        if (cache) write_csv_arrow(results[[index]], env$bundle_file)
        retrieved <<- retrieved + 1
        if (verbose) cat("\r", retrieved, "of", length(bundles), "bundles received           ")
      } else {
        result <- list(message = rawToChar(res$content))
        if (substring(result$message, 1, 1) == "{") result <- jsonlite::fromJSON(result$message)
        if (!is.null(result$code) && result$code == 1420 && retry_limit > 0) {
          retry_limit <- retry_limit - 1
          handler <- curl::new_handle(
            url = endpoint, httpauth = 1, userpwd = paste0(key, ":", secret),
            copypostfields = jsonlite::toJSON(bundles[[i]], auto_unbox = TRUE)
          )
          curl::handle_setheaders(handler, "Content-Type" = "application/json")
          curl::multi_add(handler, done = done, fail = failure, pool = pool)
          if (verbose) message("retrying batch ", index)
          Sys.sleep(1)
          curl::multi_run(pool = pool)
        } else {
          stop(paste0(if (length(result$code)) paste0(result$code, " (", result$code, "): "), result$message), call. = FALSE)
        }
      }
    }
    environment(env$done) <- env
    env$handler <- curl::new_handle(
      url = endpoint, httpauth = 1, userpwd = paste0(key, ":", secret),
      copypostfields = jsonlite::toJSON(bundles[[i]], auto_unbox = TRUE)
    )
    curl::handle_setheaders(env$handler, "Content-Type" = "application/json")
    curl::multi_add(env$handler, done = env$done, fail = failure, pool = pool)
  }
  curl::multi_run(pool = pool)
  if (verbose) cat("\r", retrieved, "of", length(bundles), "bundles received           \n")

  # prepare final results
  final_res <- do.call(rbind, results)
  if (!is.null(output)) {
    if (verbose) message("writing final result: ", output)
    dir.create(dirname(output), FALSE, TRUE)
    write_csv_arrow(final_res, output)
  }
  invisible(as.data.frame(final_res))
}
