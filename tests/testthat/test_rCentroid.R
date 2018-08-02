library(checkmate)
context("rCentroid")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  bin <- rBinarise(input, thresh = 30)
  patches <- rPatches(bin)

  output <- rCentroid(obj = patches)
  expect_class(output, "RasterLayer")
  
  output <- rCentroid(obj = bin)
  expect_class(output, "RasterLayer")
})

test_that("output is valid geometry", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rCentroid(obj = patches, output = "geom")
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
})

test_that("output is named", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rCentroid(obj = patches)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))
  dim1 <- dim(input)

  output <- rCentroid(obj = patches)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  patches <- rPatches(rBinarise(input, thresh = 30))

  expect_error(rCentroid(obj = mat))
  expect_error(rCentroid(obj = "bla"))
  expect_error(rCentroid(obj = patches, output = "bla"))
  expect_error(rCentroid(obj = patches, background = "bla"))
  expect_error(rCentroid(obj = patches, output = 1.1))
})

test_that("history is correct", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))
  
  output <- rCentroid(obj = patches)
  history <- output@history
  expect_list(history, len = 4)
  expect_equal(history[[4]], "the centroids of patches have been determined")
  
  patches@history <- list()
  output <- rCentroid(obj = patches)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the centroids of patches have been determined")
})
