.onLoad <- function(lib, pkg) {
  if (Sys.getenv("RECEPTIVITI_URL") == "") Sys.setenv(RECEPTIVITI_URL = "https://api.receptiviti.com/")
}

#' Receptiviti API
#'
#' The main function to access the \href{https://www.receptiviti.com}{Receptiviti} API.
#'
#' @param text A character vector with text to be processed.
#' @param output Path to a \code{.csv} file to write results to. If this already exists, it will be loaded instead of
#' processing any text.
#' @param key API Key; defaults to \code{Sys.getenv("RECEPTIVITI_KEY")}.
#' @param secret API Secret; defaults to \code{Sys.getenv("RECEPTIVITI_SECRET")}.
#' @param url API endpoint; defaults to \code{Sys.getenv("RECEPTIVITI_URL")}, which defaults to
#' \code{"https://api.receptiviti.com/"}.
#' @param retry_limit Maximum number of times each request can be retried after hitting a rate limit.
#' @param cache Logical; if \code{FALSE}, will not temporarily store responses, to avoid making the same requests multiple times.
#' @param verbose Logical; if \code{TRUE}, will show status messages.
#' @returns A \code{data.frame} with columns for \code{text} (the originally entered text), \code{response_id}, \code{language},
#' and \code{version} (metadata from the API), and scores from each included framework (e.g., \code{summary.word_count} and \code{liwc.i}).
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
#' @export

receptiviti <- function(text, output = NULL, key = Sys.getenv("RECEPTIVITI_KEY"), secret = Sys.getenv("RECEPTIVITI_SECRET"),
                        url = Sys.getenv("RECEPTIVITI_URL"), retry_limit = 10, cache = TRUE, verbose = FALSE) {
  # check input
  if (!is.null(output)) {
    output <- paste0(sub("\\.csv.*$", "", output), ".csv")
    if (file.exists(output)) {
      if (verbose) message("reading in existing output")
      return(read.csv(gzfile(output)))
    }
  }
  if (missing(text)) stop("enter text as the first argument", call. = FALSE)
  if (!is.character(text)) stop("text must be a character vector", call. = FALSE)
  if (key == "") stop("specify your key, or set it to the RECEPTIVITI_KEY environment variable", call. = FALSE)
  if (secret == "") stop("specify your secret, or set it to the RECEPTIVITI_SECRET environment variable", call. = FALSE)
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
  for (t in text) if (!is.na(t) && t != "") {
    entry <- list(content = t)
    size <- size + object.size(entry)
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

  if (cache) {
    temp <- paste0(tempdir(), "/receptiviti/")
    dir.create(temp, FALSE)
  }

  # make request(s)
  endpoint <- paste0(url, "framework/bulk")
  results <- list()
  pool <- curl::new_pool()
  done <- NULL
  failure <- function(message) stop("request failed: ", message)
  for (i in seq_along(bundles)) {
    if (cache) {
      bundle_file <- paste0(temp, digest::digest(bundles[[i]]), ".csv.xz")
      if (file.exists(bundle_file)) {
        if (verbose) message("loading bundle ", i, " from cache")
        results[[i]] <- read.csv(gzfile(bundle_file))
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
          result[, exclude_cols[1:3]],
          as.data.frame(unlist(result[, !names(result) %in% exclude_cols], recursive = FALSE))
        )
        if (cache) write.csv(results[[index]], xzfile(env$bundle_file), row.names = FALSE)
      } else {
        result <- list(message = rawToChar(res$content))
        if (substring(result$message, 1, 1) == "{") result <- jsonlite::fromJSON(result$message)
        if (!is.null(result$code) && result$code == 1420) {
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

  # prepare final results
  final_res <- do.call(rbind, results)
  if (!is.null(output)) write.csv(final_res, output, row.names = FALSE)
  invisible(final_res)
}
