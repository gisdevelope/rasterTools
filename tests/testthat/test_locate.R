library(checkmate)
library(testthat)
context("locate")


test_that("make tests", {
  input <- rtData$continuous
  visualise(input)
  output <- locate(1)
  expect_data_frame(output)
})
