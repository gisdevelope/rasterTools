library(checkmate)
library(testthat)
context("locate")


test_that("make tests", {
  input <- rtData$continuous
  visualise(input)
  
  output <- locate(samples = 1)
  expect_data_frame(output, ncols = 3)
})

test_that("identify works", {
  input <- rtData$continuous
  visualise(input)
  
  output <- locate(samples = 1, identify = TRUE)
  expect_data_frame(output, ncols = 5)
})

test_that("snap works", {
  # input <- rtData$continuous
  # visualise(input)
  # 
  # output <- locate(samples = 1, raster = input, snap = TRUE)
  # expect_data_frame(output, ncols = 3)
  # expect_true(output$x %% 1 == 0.5)
  # expect_true(output$y %% 1 == 0.5)
})