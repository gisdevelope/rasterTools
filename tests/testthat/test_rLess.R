library(checkmate)
context("rLess")


test_that("output has class RasterLayer", {
  input <- rtData$continuous

  output <- rLess(obj = input, thresh = 50)
  expect_class(output, "RasterLayer")

  output <- rLess(obj = input, thresh = 50, background = 0)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous

  output <- rLess(obj = input, thresh = 50)
  expect_named(output)

  output <- rLess(obj = input, thresh = 50, background = 0)
  expect_class(output, "RasterLayer")
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

  output <- rLess(obj = input, thresh = 50)
  expect_equal(dim(output), dim1)

  output <- rLess(obj = input, thresh = 50, background = 0)
  expect_class(output, "RasterLayer")
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)

  expect_error(rLess(obj = "bla", thresh = 50))
  expect_error(rLess(obj = mat, thresh = 50))
  expect_error(rLess(obj = input, thresh = 50, background = 1.1))
})

test_that("history is correct", {
  input <- rtData$continuous

  output <- rLess(obj = input, thresh = 50)
  history <- output@history
  expect_list(history, types = "character", len = 2)
})