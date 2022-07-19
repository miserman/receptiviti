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

test_that("cache works", {
  expect_message(receptiviti("a text to score", verbose = TRUE), "loading bundle 1 from cache")
  expect_message(receptiviti("a text to score", output, verbose = TRUE), "reading in existing output")
})

skip_if(!grepl("R_LIBS", getwd(), fixed = TRUE), "not making bigger requests")

test_that("bundle splitting works", {
  texts <- rep("a text to score", 2000)
  scores <- receptiviti(texts, cache = FALSE)
  expect_identical(texts, scores$text)
  expect_true(all(scores[1, -(1:2)] == scores[2000, -(1:2)]))
})
