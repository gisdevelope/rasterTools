library(checkmate)
context("rPatches")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  kernel <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3)

  output <- rPatches(obj = binarised)
  expect_class(output, "RasterLayer")

  output <- rPatches(obj = binarised, kernel = kernel)
  expect_class(output, "RasterLayer")

  output <- rPatches(obj = binarised, background = 0)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  kernel <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3)

  output <- rPatches(obj = binarised)
  expect_named(output)

  output <- rPatches(obj = binarised, kernel = kernel)
  expect_named(output)

  output <- rPatches(obj = binarised, background = 0)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  binarised <- rBinarise(input, thresh = 30)
  kernel <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3)

  output <- rPatches(obj = binarised)
  expect_equal(dim(output), dim1)

  output <- rPatches(obj = binarised, kernel = kernel)
  expect_equal(dim(output), dim1)

  output <- rPatches(obj = binarised, background = 0)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  binarised <- rBinarise(input, thresh = 30)
  kernel <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3)

  expect_error(rPatches(obj = "bla"))
  expect_error(rPatches(obj = mat))
  expect_error(rPatches(obj = binarised, kernel = c(1, 2, 3)))
  expect_error(rPatches(obj = binarised, kernel = "bla"))
  expect_error(rPatches(obj = binarised, background = 1.1))
})

test_that("history is correct", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rPatches(obj = binarised)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "patches have been determined")
})