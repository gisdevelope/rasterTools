library(testthat)
library(checkmate)
context("rReduce")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))
  myPatches <- rSegregate(patches)

  output <- rReduce(myPatches)
  expect_class(output, "RasterLayer")
})

test_that("output has class RasterStack", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))
  myPatches <- rSegregate(patches)
  by <- list(c(1:14), c(15:28))

  output <- rReduce(myPatches, by = by)
  expect_class(output, "RasterStack")
})

test_that("output is named", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))
  myPatches <- rSegregate(patches)
  by <- list(c(1:14), c(15:28))

  output <- rReduce(myPatches)
  expect_named(output)

  output <- rReduce(myPatches, by = by)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  patches <- rPatches(rBinarise(input, thresh = 30))
  myPatches <- rSegregate(patches)
  by <- list(c(1:14), c(15:28))

  output <- rReduce(myPatches)
  expect_equal(dim(output), dim1)

  output <- rReduce(myPatches, by = by)
  expect_equal(dim(output)[c(1, 2)], dim1[c(1, 2)])
})

test_that("output has less elements than input", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))
  myPatches <- rSegregate(patches)
  by <- list(c(1:14), c(15:28))

  output <- rReduce(myPatches)
  expect_lt(dim(output)[3], dim(myPatches)[3])

  output <- rReduce(myPatches, by = by)
  expect_lt(dim(output)[3], dim(myPatches)[3])
  expect_true(dim(output)[3] == 2)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  patches <- rPatches(rBinarise(input, thresh = 30))
  myPatches <- rSegregate(patches)
  by <- list(c(1:14), c(15:28))

  expect_error(rReduce(obj = "bla"))
  expect_error(rReduce(obj = mat))
  expect_error(rReduce(obj = input))
  expect_error(rReduce(obj = myPatches, by = 1))
  expect_error(rReduce(obj = myPatches, fun = "sum"))
  expect_error(rReduce(obj = myPatches, weights = "bla"))
  expect_error(rReduce(obj = myPatches, direction = 1))
})

test_that("history is correct", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))
  myPatches <- rSegregate(patches)

  output <- rReduce(myPatches)
  history <- output@history
  expect_list(history, len = 5, types = "character")
})