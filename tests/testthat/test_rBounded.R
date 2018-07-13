library(checkmate)
context("rBounded")


test_that("output has class RasterLayer", {
  input <- rtData$continuous

  output <- rBounded(obj = input, range = c(40, 60))
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous

  output <- rBounded(obj = input, range = c(40, 60))
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

 output <- rBounded(obj = input, range = c(40, 60))
 expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)

  expect_error(rBounded(obj = mat, range = c(40, 60)))
  expect_error(rBounded(obj = input))
  expect_error(rBounded(obj = "bla", range = c(40, 60)))
  expect_error(rBounded(obj = input, range = "bla"))
})

test_that("history is correct", {
  input <- rtData$continuous

  output <- rBounded(obj = input, range = c(40, 60))
  history <- output@history
  expect_list(history, types = "character", len = 2)
})