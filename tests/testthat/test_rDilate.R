library(checkmate)
context("rDilate")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rDilate(obj = binarised)
  expect_class(output, "RasterLayer")
})

test_that("dilate also non-binary input/kernel", {
  input <- rtData$continuous
  output <- rDilate(obj = input)
  expect_class(output, "RasterLayer")
  
  output <- rDilate(obj = input,
                    kernel = matrix(c(1, 1, 1, 1, 6, 1, 1, 1, 1), 3, 3))
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rDilate(obj = binarised)
  expect_named(output)
})

test_that("output is binary", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rDilate(obj = binarised)
  vals <- unique(values(output))
  expect_set_equal(vals, c(0, 1))
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  binarised <- rBinarise(input, thresh = 30)

  output <- rDilate(obj = binarised)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  binarised <- rBinarise(input, thresh = 30)

  expect_error(rDilate(obj = mat))
  expect_error(rDilate(obj = "bla"))
  expect_error(rDilate(obj = binarised, kernel = c(1, 2, 3)))
})

test_that("history is correct", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rDilate(obj = binarised)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "the raster has been morphologically dilated")
  
  binarised@history <- list()
  output <- rDilate(obj = binarised)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the raster has been morphologically dilated")
})
