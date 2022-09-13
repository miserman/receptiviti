options(stringsAsFactors = FALSE)
key <- Sys.getenv("RECEPTIVITI_KEY")
secret <- Sys.getenv("RECEPTIVITI_SECRET")
text <- "a text to score"
temp <- tempdir()
temp_cache <- paste0(temp, "/temp_cache")
Sys.setenv(RECEPTIVITI_KEY = 123, RECEPTIVITI_SECRET = 123)
on.exit(Sys.setenv(RECEPTIVITI_KEY = key, RECEPTIVITI_SECRET = secret))

test_that("invalid inputs are caught", {
  expect_error(receptiviti(), "enter text as the first argument", fixed = TRUE)
  expect_error(receptiviti("", key = ""), "specify your key", fixed = TRUE)
  expect_error(receptiviti("", key = 123), "401 (1411): Unrecognized API key pair.", fixed = TRUE)
  expect_error(receptiviti("", secret = ""), "specify your secret", fixed = TRUE)
  expect_error(receptiviti(matrix(0, 2, 2)), "text has dimensions, but no text_column column", fixed = TRUE)
  expect_error(receptiviti("", id = 1:2), "id is not the same length as text", fixed = TRUE)
  expect_error(receptiviti(c("", ""), id = c(1, 1)), "id contains duplicate values", fixed = TRUE)
  expect_error(
    receptiviti(NA, text_as_paths = TRUE),
    "NAs are not allowed in text when being treated as file paths",
    fixed = TRUE
  )
  expect_error(receptiviti("", text_as_paths = TRUE), "not all of the files in text exist", fixed = TRUE)
})

test_that("errors given existing results", {
  file <- tempfile(fileext = ".csv")
  write.csv(matrix(1), file, row.names = FALSE)
  expect_error(receptiviti(output = file), "output file already exists", fixed = TRUE)
})

skip_if(key == "", "no API key")
Sys.setenv(RECEPTIVITI_KEY = key, RECEPTIVITI_SECRET = secret)
output <- paste0(tempdir(), "/single_text.csv")

test_that("default cache works", {
  expect_identical(
    receptiviti("a text to score", cache = "", make_request = FALSE)$summary.word_count,
    4L
  )
})

test_that("invalid texts are caught", {
  expect_error(
    receptiviti(paste(rep(" ", 1e7), collapse = "")),
    "one of your texts is over the individual size limit (10 MB)",
    fixed = TRUE
  )
  expect_error(receptiviti(NA), "no valid texts to process", fixed = TRUE)
})

test_that("make_request works", {
  expect_error(
    receptiviti("a text to score", cache = FALSE, request_cache = FALSE, make_request = FALSE),
    "make_request is FALSE, but there are texts with no cached results",
    fixed = TRUE
  )
})

test_that("a single text works", {
  score <- receptiviti("a text to score", output, overwrite = TRUE, cache = temp_cache, clear_cache = TRUE)
  expect_equal(
    score[, c("social_dynamics.clout", "disc_dimensions.bold_assertive_outgoing")],
    data.frame(social_dynamics.clout = 63.646087, disc_dimensions.bold_assertive_outgoing = 68.137361)
  )
  expect_true(file.exists(output))
  expect_identical(read.csv(output), score)
})

test_that("framework selection works", {
  score <- receptiviti(text, cache = temp_cache)
  expect_identical(
    receptiviti(text, frameworks = c("summary", "liwc"), framework_prefix = TRUE, cache = temp_cache),
    score[, grep("^(?:text_|summary|liwc)", colnames(score))]
  )
  options(receptiviti_frameworks = "summary")
  expect_identical(
    receptiviti(text, framework_prefix = TRUE, cache = temp_cache),
    score[, grep("^(?:text_|summary)", colnames(score))]
  )
  options(receptiviti_frameworks = "all")
  expect_warning(
    receptiviti(text, frameworks = "x", cache = temp_cache),
    "frameworks did not match any columns -- returning all",
    fixed = TRUE
  )
})

test_that("framework prefix removal works", {
  score <- receptiviti(text, cache = temp_cache)
  colnames(score) <- sub("^.+\\.", "", colnames(score))
  expect_identical(receptiviti(text, cache = temp_cache, framework_prefix = FALSE), score)
})

test_that("as_list works", {
  score_list <- receptiviti(matrix(text), cache = FALSE, as_list = TRUE)
  expect_identical(score_list$personality, receptiviti(text, cache = temp_cache, frameworks = "personality"))
})

test_that("compression works", {
  compressed_output <- tempfile(fileext = ".csv")
  scores <- receptiviti(text, compressed_output, cache = temp_cache, compress = TRUE)
  expect_true(file.exists(paste0(compressed_output, ".xz")))
})

test_that("NAs and empty texts are handled, and IDs align", {
  id <- paste0("id", rnorm(5))
  score <- receptiviti(
    c("", "a text to score", NA, "a text to score", NA),
    id = id, cache = FALSE
  )
  expect_identical(score$id, id)
  expect_identical(score$summary.word_count, c(NA, 4L, NA, 4L, NA))
})

test_that("repeated texts works", {
  texts <- rep("a text to score", 2000)
  scores <- receptiviti(texts, return_text = TRUE, cache = FALSE)
  expect_identical(texts, scores$text)
  expect_true(all(scores[1, -(1:2)] == scores[2000, -(1:2)]))
})

test_that("later invalid inputs are caught", {
  expect_error(receptiviti(" ", text_column = ""), "text_column is specified, but text has no columns", fixed = TRUE)
  expect_error(receptiviti(" ", id_column = ""), "id_column is specified, but text has no columns", fixed = TRUE)
  expect_error(receptiviti(matrix(0, 2), text_column = ""), "text_column not found in text", fixed = TRUE)
  expect_error(receptiviti(matrix(0, 2), id_column = ""), "id_column not found in text", fixed = TRUE)
})

skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making bigger requests")

words <- vapply(seq_len(200), function(w) {
  paste0(sample(letters, sample.int(9, 1)), collapse = "")
}, "")
texts <- vapply(seq_len(50), function(d) {
  paste0(sample(words, sample.int(100, 1), TRUE), collapse = " ")
}, "")
temp_output <- tempfile(fileext = ".csv")
initial <- NULL
temp_source <- paste0(temp, "/temp_store/")
dir.create(temp_source, FALSE)
text_seq <- seq_along(texts)
files_txt <- paste0(temp_source, text_seq, ".txt")

test_that("verbose works", {
  expect_identical(
    sub(" \\([0-9.]+\\)", "", capture.output(
      initial <<- receptiviti(
        texts, temp_output,
        cache = temp_cache, clear_cache = TRUE, verbose = TRUE, overwrite = TRUE
      ),
      type = "message"
    )),
    c(
      "pinging API",
      "preparing text",
      "prepared text in 1 bundle",
      "processing bundle sequentially",
      "done retrieving; preparing final results",
      "checking cache",
      "initializing cache with 50 results",
      "preparing output",
      paste("writing results to file:", temp_output),
      "done"
    )
  )
})

test_that("cache updating and acceptable alternates are handled", {
  expect_identical(
    sub(" \\([0-9.]+\\)", "", capture.output(receptiviti(
      data.frame(text = as.factor(paste0(sample(words, sample.int(100, 1), TRUE), collapse = " "))),
      text_column = "text", verbose = TRUE, cache = temp_cache, retry_limit = FALSE, bundle_size = FALSE
    ), type = "message")),
    c(
      "pinging API",
      "preparing text",
      "prepared text in 1 bundle",
      "processing bundle sequentially",
      "done retrieving; preparing final results",
      "checking cache",
      "updating cache with 1 result",
      "preparing output",
      "done"
    )
  )
})

test_that("return is consistent between sources", {
  # from request cache
  expect_equal(receptiviti(texts, cache = FALSE), initial)

  # from main cache
  expect_equal(receptiviti(texts, cache = temp_cache, request_cache = FALSE), initial)
})

test_that("parallelization methods are consistent", {
  # sequential
  expect_equal(
    receptiviti(texts, cache = temp_cache, bundle_size = 25, cores = 1), initial
  )

  # parallel
  expect_equal(
    receptiviti(texts, cache = temp_cache, bundle_size = 10), initial
  )

  # parallel from disc
  expect_equal(
    receptiviti(texts, cache = temp_cache, bundle_size = 10, in_memory = FALSE), initial
  )

  # sequential future
  expect_equal(
    receptiviti(texts, cache = temp_cache, bundle_size = 25, use_future = TRUE), initial
  )
})

test_that("reading from files works", {
  file_txt <- paste0(temp, "/texts.txt")
  file_csv <- paste0(temp, "/texts.csv")
  writeLines(texts, file_txt)
  csv_data <- data.frame(id = text_seq, text = texts)
  arrow::write_csv_arrow(csv_data, file_csv)
  files_csv <- paste0(temp_source, text_seq, ".csv")
  for (i in text_seq) {
    writeLines(texts[i], files_txt[i])
    arrow::write_csv_arrow(data.frame(id = i, text = texts[i]), files_csv[i])
  }
  writeLines(texts[1], paste0(temp_source, "0.txt"))

  txt_directory <- receptiviti(temp_source, cache = temp_cache, bundle_size = 25)
  expect_true(all(initial$text_hash %in% txt_directory$text_hash))
  expect_false(anyDuplicated(txt_directory$text_hash) == 0)
  txt_directory <- txt_directory[!duplicated(txt_directory$text_hash), ]
  rownames(txt_directory) <- txt_directory$text_hash
  txt_directory <- txt_directory[initial$text_hash, ]
  rownames(txt_directory) <- NULL
  expect_equal(txt_directory, initial)

  expect_error(
    receptiviti(temp_source, file_type = "csv", cache = temp_cache),
    "files appear to be csv, but no text_column was specified",
    fixed = TRUE
  )
  arrow::write_csv_arrow(data.frame(text = NA), paste0(temp_source, "!.csv"))
  csv_directory <- receptiviti(temp_source, text_column = "text", file_type = "csv", cache = temp_cache)
  expect_true(all(initial$text_hash %in% csv_directory$text_hash))
  csv_directory <- csv_directory[!is.na(csv_directory$text_hash), ]
  rownames(csv_directory) <- csv_directory$text_hash
  csv_directory <- csv_directory[initial$text_hash, ]
  rownames(csv_directory) <- NULL
  expect_equal(csv_directory, initial)

  expect_equal(receptiviti(file_txt, cache = temp_cache, in_memory = FALSE)[, -1], initial)
  expect_equal(
    receptiviti(file_txt, collapse_lines = TRUE, cache = temp_cache),
    receptiviti(paste(texts, collapse = " "), cache = temp_cache)
  )
  expect_error(receptiviti(file_csv, cache = temp_cache))
  expect_equal(receptiviti(file_csv, text_column = "text", cache = temp_cache)[, -1], initial)

  alt_id <- receptiviti(file_csv, text_column = "text", id_column = "id", cache = temp_cache)
  expect_identical(alt_id, receptiviti(csv_data, text_column = "text", id_column = "id", cache = temp_cache))
  expect_identical(alt_id, receptiviti(csv_data$text, id = csv_data$id, cache = temp_cache))
})

test_that("spliting oversized bundles works", {
  texts <- vapply(seq_len(50), function(d) {
    paste0(sample(words, 6e4, TRUE), collapse = " ")
  }, "")
  for (i in seq_along(texts)) {
    writeLines(texts[i], files_txt[i])
  }
  expect_true(sum(file.size(files_txt)) > 1e7)
  expect_error(receptiviti(temp_source, cache = FALSE), NA)
})

test_that("rate limit is handled", {
  texts <- vapply(seq_len(50), function(d) {
    paste0(sample(words, 5, TRUE), collapse = " ")
  }, "")
  expect_error(
    receptiviti(texts, bundle_size = 1, request_cache = FALSE, cache = FALSE, cores = 1, retry_limit = 0),
    "Rate limit exceeded"
  )
  expect_identical(
    receptiviti(texts, bundle_size = 1, request_cache = FALSE, cache = FALSE, cores = 1)$summary.word_count,
    rep(5L, 50)
  )
})
