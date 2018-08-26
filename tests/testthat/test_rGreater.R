library(checkmate)
library(testthat)
context("rGreater")


test_that("output has class RasterLayer", {
  input <- rtData$continuous

  output <- rGreater(obj = input, thresh = 50)
  expect_class(output, "RasterLayer")

  output <- rGreater(obj = input, thresh = 50, background = 0)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous

  output <- rGreater(obj = input, thresh = 50)
  expect_named(output)

  output <- rGreater(obj = input, thresh = 50, background = 0)
  expect_class(output, "RasterLayer")
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

  output <- rGreater(obj = input, thresh = 50)
  expect_equal(dim(output), dim1)

  output <- rGreater(obj = input, thresh = 50, background = 0)
  expect_class(output, "RasterLayer")
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)

  expect_error(rGreater(obj = "bla", thresh = 50))
  expect_error(rGreater(obj = mat, thresh = 50))
  expect_error(rGreater(obj = input, thresh = 50, background = 1.1))
  expect_error(rGreater(obj = input, thresh = 101))
})

test_that("history is correct", {
  input <- rtData$continuous

  output <- rGreater(obj = input, thresh = 50)
  history <- output@history
  expect_list(history, types = "character", len = 2)
  
  input@history <- list("this object has a history")
  output <- rGreater(obj = input, thresh = 50)
  history <- output@history
  expect_list(history, types = "character", len = 2)
  expect_true(history[[1]] == "this object has a history")
})