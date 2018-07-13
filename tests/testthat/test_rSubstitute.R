library(checkmate)
context("rSubstitute")


test_that("output has class RasterLayer", {
  input <- rtData$continuous

  output <- rSubstitute(input, old = c(41:47), new = 40)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous

  output <- rSubstitute(input, old = c(41:47), new = 40)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

  output <- rSubstitute(input, old = c(41:47), new = 40)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)

  expect_error(rSubstitute(input))
  expect_error(rSubstitute(input, old = c(41:47)))
  expect_error(rSubstitute(mat, old = c(41:47), new = 40))
  expect_error(rSubstitute("bla", old = c(41:47), new = 40))
  expect_error(rSubstitute(input, old = c(41:47), new = 40.1))
  expect_error(rSubstitute(input, old = 41.1, new = 40))
  expect_error(rSubstitute(input, old = 41.1, new = "bla"))
  expect_error(rSubstitute(input, old = "bla", new = 40))
})

test_that("history is correct", {
  input <- rtData$continuous

  output <- rSubstitute(input, old = c(41:47), new = 40)
  history <- output@history
  expect_list(history, len = 2, types = "character")
})