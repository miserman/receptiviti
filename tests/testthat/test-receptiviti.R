file <- tempfile(fileext = ".csv")
data <- data.frame(id = "t1", a = 1L, b.c = 3.5, b.c.e = 4.4)

test_that("over-sized text is caught", {
  expect_error(
    receptiviti(paste(rep(" ", 1e7), collapse = "")),
    "one of your texts is over the individual size limit (10 MB)",
    fixed = TRUE
  )
})

test_that("invalid inputs are caught", {
  expect_error(receptiviti(), "enter text as the first argument", fixed = TRUE)
  expect_error(
    receptiviti("", key = ""),
    "specify your key, or set it to the RECEPTIVITI_KEY environment variable",
    fixed = TRUE
  )
  expect_error(
    receptiviti("", key = "123"),
    "401 (1411): Unrecognized API key pair.",
    fixed = TRUE
  )
  expect_error(
    receptiviti("", secret = ""),
    "specify your secret, or set it to the RECEPTIVITI_SECRET environment variable",
    fixed = TRUE
  )
  expect_error(receptiviti(matrix(0, 2, 2)), "text has dimensions, but no text_column column", fixed = TRUE)
  expect_error(receptiviti("", id = 1:2), "id is not the same length as text", fixed = TRUE)
  expect_error(receptiviti(c("", ""), id = c(1, 1)), "id contains duplicate values", fixed = TRUE)
  expect_error(receptiviti(NA), "no valid texts to process", fixed = TRUE)
  expect_error(
    receptiviti(NA, text_as_paths = TRUE),
    "NAs are not allowed in text when being treated as file paths",
    fixed = TRUE
  )
  expect_error(receptiviti("", text_as_paths = TRUE), "not all of the files in text exist", fixed = TRUE)
})

test_that("loading existing results works", {
  write.csv(data, file, row.names = FALSE)
  expect_identical(receptiviti(output = file), data)
})

test_that("framework selection works", {
  expect_identical(receptiviti(output = file, frameworks = "b"), data[, -2])
  expect_warning(
    receptiviti(output = file, frameworks = "x"),
    "frameworks did not match any columns -- returning all",
    fixed = TRUE
  )
})

test_that("framework prefix removal works", {
  colnames(data) <- sub("^.+\\.", "", colnames(data))
  expect_identical(receptiviti(output = file, framework_prefix = FALSE), data)
})

skip_if(Sys.getenv("RECEPTIVITI_KEY") == "", "no API key")
output <- paste0(tempdir(), "/single_text.csv")

test_that("make_request works", {
  expect_error(
    receptiviti("a text to score", cache = FALSE, request_cache = FALSE, make_request = FALSE),
    "make_request is FALSE, but there are texts with no cached results",
    fixed = TRUE
  )
})

test_that("a single text works", {
  score <- receptiviti("a text to score", output, cache = FALSE)
  expect_equal(
    score[, c("social_dynamics.clout", "disc_dimensions.bold_assertive_outgoing")],
    data.frame(social_dynamics.clout = 63.646087, disc_dimensions.bold_assertive_outgoing = 68.137361)
  )
  expect_true(file.exists(output))
  expect_identical(read.csv(output), score)
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

test_that("verbosely loading an existing file works", {
  expect_identical(
    sub(" \\([0-9.]+\\)", "", capture.output(receptiviti(output = output, verbose = TRUE), type = "message")),
    c("reading in existing output", "done")
  )
})

test_that("repeated texts works", {
  texts <- rep("a text to score", 2000)
  scores <- receptiviti(texts, return_text = TRUE, cache = FALSE)
  expect_identical(texts, scores$text)
  expect_true(all(scores[1, -(1:2)] == scores[2000, -(1:2)]))
})

skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making bigger requests")

words <- vapply(seq_len(200), function(w) {
  paste0(sample(letters, sample.int(9, 1)), collapse = "")
}, "")
texts <- vapply(seq_len(50), function(d) {
  paste0(sample(words, sample.int(100, 1), TRUE), collapse = " ")
}, "")
temp <- tempdir()
temp_cache <- paste0(temp, "/temp_cache")
temp_output <- tempfile(fileext = ".csv")
initial <- NULL

test_that("verbose works", {
  expect_identical(
    sub(" \\([0-9.]+\\)", "", capture.output(
      initial <<- receptiviti(texts, temp_output, cache = temp_cache, verbose = TRUE),
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
  # from file
  expect_equal(receptiviti(output = temp_output), initial)

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
  text_seq <- seq_along(texts)
  temp_source <- paste0(temp, "/temp_store/")
  dir.create(temp_source, FALSE)
  file_txt <- paste0(temp, "/texts.txt")
  file_csv <- paste0(temp, "/texts.csv")
  writeLines(texts, file_txt)
  arrow::write_csv_arrow(data.frame(id = text_seq, text = texts), file_csv)
  files_txt <- paste0(temp_source, text_seq, ".txt")
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

  expect_equal(receptiviti(file_txt, cache = temp_cache)[, -1], initial)
  expect_equal(
    receptiviti(file_txt, collapse_lines = TRUE, cache = temp_cache),
    receptiviti(paste(texts, collapse = " "), cache = temp_cache)
  )
  expect_error(receptiviti(file_csv, cache = temp_cache))
  expect_equal(receptiviti(file_csv, text_column = "text", cache = temp_cache)[, -1], initial)
})
