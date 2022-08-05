file <- tempfile(fileext = ".csv")
data <- data.frame(id = "t1", a = 1L, b.c = 3.5, b.c.e = 4.4)

test_that("over-sized text is caught", {
  expect_error(receptiviti(paste(rep(" ", 1e7), collapse = "")))
})

test_that("loading existing results works", {
  write.csv(data, file, row.names = FALSE)
  expect_identical(receptiviti(output = file), data)
})

test_that("framework selection works", {
  expect_identical(receptiviti(output = file, frameworks = "b"), data[, -2])
})

test_that("framework prefix removal works", {
  colnames(data) <- sub("^.+\\.", "", colnames(data))
  expect_identical(receptiviti(output = file, framework_prefix = FALSE), data)
})

skip_if(Sys.getenv("RECEPTIVITI_KEY") == "", "no API key")
output <- paste0(tempdir(), "/single_text.csv")

test_that("a single text works", {
  score <- receptiviti("a text to score", output)
  expect_equal(
    score[, c("social_dynamics.clout", "disc_dimensions.bold_assertive_outgoing")],
    data.frame(social_dynamics.clout = 63.646087, disc_dimensions.bold_assertive_outgoing = 68.137361)
  )
  expect_true(file.exists(output))
  expect_equal(read.csv(output), score)
})

test_that("NAs and empty texts are handled, and IDs align", {
  id <- paste0("id", rnorm(5))
  score <- receptiviti(
    c("", "a text to score", NA, "a text to score", NA),
    id = id
  )
  expect_identical(score$id, id)
  expect_identical(score$summary.words_per_sentence, c(NA, 4, NA, 4, NA))
})

test_that("verbosely loading an existing file works", {
  expect_message(receptiviti(output = output, verbose = TRUE), "reading in existing output")
})

test_that("repeated texts works", {
  texts <- rep("a text to score", 2000)
  scores <- receptiviti(texts, return_text = TRUE)
  expect_identical(texts, scores$text)
  expect_true(all(scores[1, -(1:2)] == scores[2000, -(1:2)]))
})

skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making bigger requests")

words <- vapply(seq_len(200), function(w) {
  paste0(sample(letters, sample(9, 1)), collapse = "")
}, "")
texts <- vapply(seq_len(50), function(d) {
  paste0(sample(words, sample(100, 1), TRUE), collapse = " ")
}, "")
temp_cache <- paste0(tempdir(), "/temp_cache")
temp_output <- tempfile(fileext = ".csv")
initial <- receptiviti(texts, temp_output, cache = temp_cache)

test_that("return is consistent between sources", {
  # from file
  expect_identical(receptiviti(output = temp_output), initial)

  # from request cache
  expect_identical(receptiviti(texts, cache = FALSE), initial)

  # from main cache
  expect_identical(receptiviti(texts, cache = temp_cache, request_cache = FALSE), initial)
})

test_that("parallelization methods are consistent", {
  # sequential
  expect_identical(
    receptiviti(texts, cache = temp_cache, bundle_size = 25, cores = 1), initial
  )

  # parallel
  expect_identical(
    receptiviti(texts, cache = temp_cache, bundle_size = 10), initial
  )

  # sequential future
  expect_identical(
    receptiviti(texts, cache = temp_cache, bundle_size = 25, use_future = TRUE), initial
  )
})
