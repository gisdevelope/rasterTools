library(checkmate)
library(testthat)
context("rDistance")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 40)

  output <- rDistance(binarised)
  expect_class(output, "RasterLayer")

  output <- rDistance(binarised, method = "manhattan")
  expect_class(output, "RasterLayer")

  output <- rDistance(binarised, method = "chessboard")
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 40)

  output <- rDistance(binarised)
  expect_named(output)

  output <- rDistance(binarised, method = "manhattan")
  expect_named(output)

  output <- rDistance(binarised, method = "chessboard")
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  binarised <- rBinarise(input, thresh = 40)

  output <- rDistance(binarised)
  expect_equal(dim(output), dim1)

  output <- rDistance(binarised, method = "manhattan")
  expect_equal(dim(output), dim1)

  output <- rDistance(binarised, method = "chessboard")
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  binarised <- rBinarise(input, thresh = 40)

  expect_error(rDistance("bla"))
  expect_error(rDistance(binarised, method = 2))
  expect_error(rDistance(binarised, method = "bla"))
})

test_that("history is correct", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 40)

  output <- rDistance(binarised)
  history <- output@history
  expect_list(history, types = "character", len = 3)
})

test_that("bibliography item has been created", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 40)

  output <- rDistance(binarised)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})
