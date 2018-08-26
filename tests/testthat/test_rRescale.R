library(checkmate)
library(mmand)
context("rRescale")


test_that("output has class RasterLayer", {
  input <- rtData$continuous

  output <- rRescale(input, factor = 0.5)
  expect_class(output, "RasterLayer")

  output <- rRescale(input, factor = 2)
  expect_class(output, "RasterLayer")

  output <- rRescale(input, factor = 2, kernelFunction = triangleKernel())
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous

  output <- rRescale(input, factor = 0.5)
  expect_named(output)

  output <- rRescale(input, factor = 2)
  expect_named(output)

  output <- rRescale(input, factor = 2, kernelFunction = triangleKernel())
  expect_named(output)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)

  expect_error(rRescale(mat, factor = 0.5))
  expect_error(rRescale("bla", factor = 0.5))
  expect_error(rRescale(input, factor = 0.5, kernelFunction = matrix(1, 1, 1)))
  expect_error(rRescale(input, factor = 0.5, kernelFunction = "box"))
})

test_that("history is correct", {
  input <- rtData$continuous

  output <- rRescale(input, factor = 0.5)
  history <- output@history
  expect_list(history, len = 2, types = "character")
  
  input@history <- list("this object has a history")
  output <- rRescale(input, factor = 0.5)
  history <- output@history
  expect_list(history, len = 2, types = "character")
  expect_true(history[[1]] == "this object has a history")
})

test_that("bibliography item has been created", {
  input <- rtData$continuous
  
  output <- rRescale(input, factor = 0.5)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")

  options(bibliography = NULL)
  output <- rRescale(input, factor = 0.5)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})