library(checkmate)
library(testthat)
library(raster)
context("rBinarise")


test_that("output has class RasterLayer", {
  input <- rtData$continuous

  # test for thresh
  output <- rBinarise(obj = input, thresh = 30)
  expect_class(output, "RasterLayer")

  # test for match
  output <- rBinarise(obj = input, match = 1)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous

  # test for thresh
  output <- rBinarise(obj = input, thresh = 30)
  expect_named(output)

  # test for match
  output <- rBinarise(obj = input, match = 1)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

  # test for thresh
  output <- rBinarise(obj = input, thresh = 30)
  expect_equal(dim(output), dim1)

  # test for match
  output <- rBinarise(obj = input, match = 1)
  expect_equal(dim(output), dim1)
})

test_that("output is binary", {
  input <- rtData$continuous

  # test for thresh
  output <- rBinarise(obj = input, thresh = 30)
  vals <- unique(values(output))
  expect_set_equal(vals, c(0, 1))

  # test for match
  output <- rBinarise(obj = input, match = 1)
  vals <- unique(values(output))
  expect_set_equal(vals, c(0, 1))
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)

  expect_error(rBinarise(obj = "bla"))
  expect_error(rBinarise(obj = mat))
  expect_error(rBinarise(obj = input, thresh = "bla"))
  expect_error(rBinarise(obj = input, thresh = 101))
  expect_error(rBinarise(obj = input, match = "bla"))
})

test_that("history is correct", {
  input <- rtData$continuous
  cat <- rCategorise(input, n = 5)

  # test for thresh
  output <- rBinarise(obj = input, thresh = 30)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the values have been binarised")

  # test for match
  output <- rBinarise(obj = input, match = 1)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the values have been binarised")
  
  # test when another modification preceeded
  output <- rBinarise(obj = cat, thresh = 4)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "the values have been binarised")
  
})