library(checkmate)
library(testthat)
library(raster)
context("rMask")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 1

  output <- rMask(obj = input, mask = m)
  expect_class(output, "RasterLayer")

  output <- rMask(obj = input, mask = m, background = 0)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 1

  output <- rMask(obj = input, mask = m)
  expect_named(output)

  output <- rMask(obj = input, mask = m, background = 0)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 1

  output <- rMask(obj = input, mask = m)
  expect_equal(dim(output), dim1)

  output <- rMask(obj = input, mask = m, background = 0)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 1

  expect_error(rMask(obj = "bla"))
  expect_error(rMask(obj = mat))
  expect_error(rMask(obj = input, mask = "bla"))
  expect_error(rMask(obj = mat, background = 0))
  expect_error(rMask(obj = mat, mask = m, background = 1.1))
})

test_that("history is correct", {
  input <- rtData$continuous
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 1


  output <- rMask(obj = input, mask = m)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the raster has been masked")
})