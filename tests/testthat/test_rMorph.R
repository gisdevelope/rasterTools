library(checkmate)
library(testthat)
library(raster)
context("rMorph")


test_that("output has class RasterLayer/-Stack", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "greater", merge = "all", rotate = FALSE, strictKernel = FALSE)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  dim1 <- dim(input)
  
  output <- rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "greater", merge = "all", rotate = FALSE, strictKernel = FALSE)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

  output <- rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "greater", merge = "all", rotate = FALSE, strictKernel = FALSE)
  expect_equal(dim(output), dim1)
})

test_that("output has the correct values (0/1) or (NA/1)", {
  input <- rtData$continuous
  
  output <- rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "greater", merge = "all", rotate = FALSE, strictKernel = FALSE)
  vals <- unique(values(output))
  expect_true(all(vals %in% c(0, 1)))
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  
  expect_error(rMorph(obj = mat))
  expect_error(rMorph(obj = input, kernel = c(40, 1, 1)))
  expect_error(rMorph(obj = input, kernel = list(c(40, 1, 1))))
  expect_error(rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "bla"))
  expect_error(rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "greater", merge = "bla"))
  expect_error(rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "greater", merge = "all", rotate = "bla"))
  expect_error(rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "greater", merge = "all", rotate = FALSE, strictKernel = "bla"))
  expect_error(rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "greater", merge = "all", rotate = FALSE, background = "bla"))
})

test_that("history is correct", {
  input <- rtData$continuous
  
  output <- rMorph(obj = input, kernel = matrix(40, 1, 1), blend = "greater", merge = "all", rotate = FALSE, 
                   strictKernel = FALSE)
  history <- output@history
  expect_list(history, len = 2, types = "character")
})