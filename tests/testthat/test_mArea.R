library(checkmate)
library(raster)
context("mArea")


test_that("output is data.frame", {
  cat <- rtData$categorial
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mArea(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 26, any.missing = FALSE)

  output <- mArea(obj = cat, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 52, any.missing = FALSE)

  output <- mArea(obj = cat, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- mArea(obj = cat, scale = "window")
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
  cat <- rtData$categorial
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mArea(obj = bin, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "cells"))

  output <- mArea(obj = cat, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "cells"))

  output <- mArea(obj = cat, scale = "class")
  expect_names(names(output), identical.to = c("class", "cells"))

  output <- mArea(obj = cat, scale = "window")
  expect_names(names(output), identical.to = c("window", "cells"))
})

test_that("output with the correct unit", {
  cat <- rtData$categorial
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mArea(obj = bin, scale = "patch", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "area"))

  output <- mArea(obj = cat, scale = "patch", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "area"))

  output <- mArea(obj = cat, scale = "class", unit = "map")
  expect_names(names(output), identical.to = c("class", "area"))

  output <- mArea(obj = cat, scale = "window", unit = "map")
  expect_names(names(output), identical.to = c("window", "area"))
})

test_that("Error if arguments have wrong value", {
  cat <- rtData$categorial
  mat <- as.matrix(cat)

  expect_error(mArea(obj = mat))
  expect_error(mArea(obj = cat, scale = "landscape"))
  expect_error(mArea(obj = cat, unit = "meter"))
  expect_error(mArea(obj = cat, layer = 1))
})

test_that("bibliography item has been created", {
  cat <- rtData$categorial

  output <- mArea(obj = cat, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})
