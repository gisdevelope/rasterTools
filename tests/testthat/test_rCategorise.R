library(checkmate)
context("rCategorise")


test_that("output has class RasterLayer", {
  input <- rtData$continuous

  output <- rCategorise(obj = input, breaks = c(25, 50, 75, 90))
  expect_class(output, "RasterLayer")

  output <- rCategorise(obj = input, n = 5)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous

  output <- rCategorise(obj = input, breaks = c(25, 50, 75, 90))
  expect_named(output)

  output <- rCategorise(obj = input, n = 5)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

  output <- rCategorise(obj = input, breaks = c(25, 50, 75, 90))
  expect_equal(dim(output), dim1)

  output <- rCategorise(obj = input, n = 5)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)

  expect_error(rCategorise(obj = "bla"))
  expect_error(rCategorise(obj = mat))
  expect_error(rCategorise(obj = input, breaks = "bla"))
  expect_error(rCategorise(obj = input, n = "bla"))
  expect_error(rCategorise(obj = input, n = 5.5))
})

test_that("history is correct", {
  input <- rtData$continuous

  output <- rCategorise(obj = input, n = 5)
  history <- output@history
  expect_list(history, types = "character", len = 2)
})