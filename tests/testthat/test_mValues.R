library(checkmate)
library(testthat)
library(raster)
context("mValues")


test_that("output is data.frame", {
  input <- rtData$continuous
  bin <- rBinarise(rtData$continuous, thresh = 40)
  patches <- rPatches(bin)
  
  output <- mValues(obj = input, param = c("mean", "sd", "iqr", "number"))
  expect_data_frame(output, ncols = 5, nrows = 1)
  
  output <- mValues(obj = raster::stack(input, patches), param = c("weighted.mean", "quantile"), layer = "continuous", groupBy = "patches")
  expect_data_frame(output, ncols = 7, nrows = 26)
  
  output <- mValues(obj = raster::stack(input, patches), param = c("all"))
  expect_data_frame(output, ncols = 16, nrows = 26)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  
  expect_error(mValues(obj = "bla"))
  expect_error(mValues(obj = input, param = "bla"))
  expect_error(mValues(obj = input, param = 1))
  expect_error(mValues(obj = input, groupBy = "bla"))
  expect_error(mValues(obj = input, groupBy = 1))
  expect_error(mValues(obj = input, layer = "bla"))
  expect_error(mValues(obj = input, layer = 1))
  
})

test_that("bibliography item has been created", {

})