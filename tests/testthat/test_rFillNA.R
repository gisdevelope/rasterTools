library(checkmate)
library(raster)
context("rFillNA")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rFillNA(patches)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rFillNA(patches)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rFillNA(patches)
  expect_equal(dim(output), dim1)
})

test_that("output does not contain 'NA'", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rFillNA(patches)
  vals <- values(output)
  expect_false(any(is.na(vals)))
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  patches <- rPatches(rBinarise(input, thresh = 30))

  expect_error(rFillNA(obj = "bla"))
  expect_error(rFillNA(obj = mat))
  expect_error(rFillNA(obj = patches, with = "bla"))
  expect_error(rFillNA(obj = patches, with = 1.1))
})

test_that("history is correct", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rFillNA(patches)
  history <- output@history
  expect_list(history, len = 4, types = "character")
})