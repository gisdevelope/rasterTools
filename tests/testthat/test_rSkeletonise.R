library(checkmate)
context("rSkeletonise")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rSkeletonise(obj = binarised)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rSkeletonise(obj = binarised)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  binarised <- rBinarise(input, thresh = 30)

  output <- rSkeletonise(obj = binarised)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  binarised <- rBinarise(input, thresh = 30)

  expect_error(rSkeletonise(obj = mat))
  expect_error(rSkeletonise(obj = "bla"))
  expect_error(rSkeletonise(obj = binarised, kernel = c(1, 2, 3)))
})

test_that("history is correct", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rSkeletonise(obj = binarised)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "the morphological skeleton has been determined")
})
