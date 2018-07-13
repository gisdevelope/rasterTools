library(checkmate)
context("rPermutate")


test_that("output has class RasterLayer", {
  input <- rtData$continuous

  output <- rPermute(obj = input)
  expect_class(output, "RasterLayer")

  output <- rPermute(obj = input, type = "revert")
  expect_class(output, "RasterLayer")

  output <- rPermute(obj = input, type = "cycle", by = 10)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous

  output <- rPermute(obj = input)
  expect_named(output)

  output <- rPermute(obj = input, type = "revert")
  expect_named(output)

  output <- rPermute(obj = input, type = "cycle", by = 10)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

  output <- rPermute(obj = input)
  expect_equal(dim(output), dim1)

  output <- rPermute(obj = input, type = "revert")
  expect_equal(dim(output), dim1)

  output <- rPermute(obj = input, type = "cycle", by = 10)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)

  expect_error(rPermute(obj = "bla"))
  expect_error(rPermute(obj = mat))
  expect_error(rPermute(obj = input, type = "bla"))
  expect_error(rPermute(obj = input, type = 1))
  expect_error(rPermute(obj = input, type = "cycle", by = "bla"))
  expect_error(rPermute(obj = input, type = "cycle", by = 2.5))
})

test_that("history is correct", {
  input <- rtData$continuous

  output <- rPermute(obj = input)
  history <- output@history
  expect_list(history, len = 2, types = "character")
})