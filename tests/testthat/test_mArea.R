library(checkmate)
library(raster)
context("mArea")


test_that("output is data.frame", {
  input <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mArea(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 26, any.missing = FALSE)

  output <- mArea(obj = input, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 52, any.missing = FALSE)

  output <- mArea(obj = input, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- mArea(obj = input, scale = "landscape")
  expect_data_frame(output, ncols = 2, nrows = 1, any.missing = FALSE)
})

test_that("determines patches, when binarised input is provided and 'scale = patch'", {
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mArea(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 26, any.missing = FALSE)

  output <- mArea(obj = bin, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 2, any.missing = FALSE)
})

test_that("output has the correct columm names", {
  input <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mArea(obj = bin, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "cells"))

  output <- mArea(obj = input, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "cells"))

  output <- mArea(obj = input, scale = "class")
  expect_names(names(output), identical.to = c("class", "cells"))

  output <- mArea(obj = input, scale = "landscape")
  expect_names(names(output), identical.to = c("landscape", "cells"))
})

test_that("output with the correct unit", {
  input <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mArea(obj = bin, scale = "patch", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "area"))

  output <- mArea(obj = input, scale = "patch", layer = "categorical", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "area"))

  output <- mArea(obj = input, scale = "class", unit = "map")
  expect_names(names(output), identical.to = c("class", "area"))

  output <- mArea(obj = input, scale = "landscape", unit = "map")
  expect_names(names(output), identical.to = c("landscape", "area"))
})

test_that("Error if arguments have wrong value", {
  input <- rtData$categorical
  mat <- as.matrix(input)

  expect_error(mArea(obj = mat))
  expect_error(mArea(obj = input, scale = "bla"))
  expect_error(mArea(obj = input, unit = "meter"))
  expect_error(mArea(obj = input, layer = 1))
})

test_that("bibliography item has been created", {
  input <- rtData$categorical
  options(bibliography = NULL)
  
  output <- mArea(obj = input, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  
  options(bibliography = NULL)
  bin <- rBinarise(rtData$continuous, thresh = 40)
  disEuc <- rDistance(bin)
  
  output <- mArea(obj = input, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})
