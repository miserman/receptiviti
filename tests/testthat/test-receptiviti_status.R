test_that("failures works", {
  expect_identical(capture.output(receptiviti_status("example.com"), type = "message")[2], "Message: 404")
  expect_identical(capture.output(receptiviti_status(key = "123"), type = "message")[2], "Message: 401 (1411): Unrecognized API key pair.")
})

skip_if(Sys.getenv("RECEPTIVITI_KEY") == "", "no API key")

test_that("success works", {
  message <- capture.output(receptiviti_status(include_headers = TRUE), type = "message")
  expect_identical(message[1], "Status: OK")
  expect_true(grepl("200", message[4], fixed = TRUE))
})
