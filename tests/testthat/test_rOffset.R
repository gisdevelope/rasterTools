library(checkmate)
context("rOffset")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rOffset(obj = binarised)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rOffset(obj = binarised)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  binarised <- rBinarise(input, thresh = 30)

  output <- rOffset(obj = binarised)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  binarised <- rBinarise(input, thresh = 30)

  expect_error(rOffset(obj = mat))
  expect_error(rOffset(obj = "bla"))
  expect_error(rOffset(obj = input, value = "bla"))
})

test_that("history is correct", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rOffset(obj = binarised)
  history <- output@history
  expect_list(history, len = 3, types = "character")
  
  binarised@history <- list()
  output <- rOffset(obj = binarised)
  history <- output@history
  expect_list(history, len = 2, types = "character")
})
