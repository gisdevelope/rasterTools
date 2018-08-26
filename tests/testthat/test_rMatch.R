library(checkmate)
library(raster)
context("rMatch")


test_that("output has class RasterLayer/-Stack", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  kIso <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3)
  kCorner <- matrix(c(NA, 1, NA, 0, 1, 1, 0, 0, NA), 3, 3)

  output <- rMatch(binarised, kernel = kIso)
  expect_class(output, "RasterLayer")

  output <- rMatch(binarised, kernel = list(kIso, kCorner))
  expect_class(output, "RasterStack")

  output <- rMatch(binarised, kernel = kIso, rotate = FALSE)
  expect_class(output, "RasterLayer")

  output <- rMatch(binarised, kernel = kIso, background = 0)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  kIso <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3)

  output <- rMatch(binarised, kernel = kIso)
  expect_named(output)

  output <- rMatch(binarised, kernel = kIso, rotate = FALSE)
  expect_named(output)

  output <- rMatch(binarised, kernel = kIso, background = 0)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  binarised <- rBinarise(input, thresh = 30)
  kIso <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3)

  output <- rMatch(binarised, kernel = kIso)
  expect_equal(dim(output), dim1)

  output <- rMatch(binarised, kernel = kIso, rotate = FALSE)
  expect_equal(dim(output), dim1)

  output <- rMatch(binarised, kernel = kIso, background = 0)
  expect_equal(dim(output), dim1)
})

test_that("output has the correct values (0/1) or (NA/1)", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  kIso <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3)

  output <- rMatch(binarised, kernel = kIso)
  vals <- unique(values(output))
  expect_true(all(vals %in% c(NA, 1)))

  output <- rMatch(binarised, kernel = kIso, background = 0)
  vals <- unique(values(output))
  expect_true(all(vals %in% c(0, 1)))
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  binarised <- rBinarise(input, thresh = 30)
  kIso <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3)

  expect_error(rMatch(obj = "bla"))
  expect_error(rMatch(obj = mat))
  expect_error(rMatch(obj = input, kernel = c(1, 2, 3)))
  expect_error(rMatch(obj = input, rotate = "bla"))
  expect_error(rMatch(obj = input, background = 1.1))
})

test_that("history is correct", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  kIso <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3)

  output <- rMatch(binarised, kernel = kIso)
  history <- output@history
  expect_list(history, len = 3, types = "character")
  
  binarised@history <- list()
  output <- rMatch(binarised, kernel = kIso)
  history <- output@history
  expect_list(history, len = 2, types = "character")
})