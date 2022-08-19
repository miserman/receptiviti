#' @rdname receptiviti
#' @export

receptiviti_status <- function(url = Sys.getenv("RECEPTIVITI_URL"), key = Sys.getenv("RECEPTIVITI_KEY"),
                               secret = Sys.getenv("RECEPTIVITI_SECRET"), verbose = TRUE) {
  if (key == "") stop("specify your key, or set it to the RECEPTIVITI_KEY environment variable", call. = FALSE)
  if (secret == "") stop("specify your secret, or set it to the RECEPTIVITI_SECRET environment variable", call. = FALSE)
  handler <- new_handle(httpauth = 1, userpwd = paste0(key, ":", secret))
  url <- paste0(sub("(?:/v\\d+)?/+$", "", url), "/v1/ping")
  ping <- curl_fetch_memory(url, handler)
  ping$content <- list(message = rawToChar(ping$content))
  if (substr(ping$content, 1, 1) == "{") ping$content <- fromJSON(ping$content$message)
  ping$status_message <- if (ping$status_code == 200) {
    ping$content$pong
  } else {
    paste0(
      if (length(ping$content$code)) paste0(ping$status_code, " (", ping$content$code, "): "),
      if (nchar(ping$content$message) > 500) ping$status_code else ping$content$message
    )
  }
  if (verbose) {
    message(ping$status_message)
    ping$headers <- strsplit(rawToChar(ping$headers), "[\r\n]+", perl = TRUE)[[1]]
    json <- regexec("\\{.+\\}", ping$headers)
    for (i in seq_along(json)) {
      if (json[[i]] != -1) {
        regmatches(ping$headers[[i]], json[[i]]) <- paste(" ", strsplit(toJSON(
          fromJSON(regmatches(ping$headers[[i]], json[[i]])),
          auto_unbox = TRUE, pretty = TRUE
        ), "\n")[[1]], collapse = "\n")
      }
    }
    message(paste0("\n", paste(" ", ping$headers, collapse = "\n")))
  }
  invisible(ping)
}
