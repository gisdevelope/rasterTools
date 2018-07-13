library(checkmate)
context("rRange")


test_that("output has class RasterLayer", {
  input <- rtData$continuous

  output <- rRange(obj = input, range = c(0, 10))
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous

  output <- rRange(obj = input, range = c(0, 10))
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

  output <- rRange(obj = input, range = c(0, 10))
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)

  expect_error(rRange(obj = "bla", range = c(0, 10)))
  expect_error(rRange(obj = mat, range = c(0, 10)))
  expect_error(rRange(obj = input, range = "bla"))
  expect_error(rRange(obj = input, range = 1))
})

test_that("history is correct", {
  input <- rtData$continuous

  output <- rRange(obj = input, range = c(0, 10))
  history <- output@history
  expect_list(history, len = 2, types = "character")
})