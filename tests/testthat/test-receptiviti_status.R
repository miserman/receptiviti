test_that("failures works", {
  expect_identical(capture.output(receptiviti_status("example.com"), type = "message")[1], "404")
  expect_identical(capture.output(receptiviti_status(key = "123"), type = "message")[1], "401 (1411): Unrecognized API key pair.")
})

skip_if(Sys.getenv("RECEPTIVITI_KEY") == "", "no API key")

test_that("success works", {
  expect_true(grepl("Hello there, ", capture.output(receptiviti_status(), type = "message")[1], fixed = TRUE))
})
