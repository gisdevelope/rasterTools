library(checkmate)
library(testthat)
context("geomRand")


test_that("output is valid geometry", {
  input <- matrix(nrow = 100, ncol = 100, data = 0)

  output <- geomRand(type = "polygon", vertices = 4)
  expect_class(output, classes = "geom")
  
  output <- geomRand(template = input, vertices = 5)
  expect_class(output, classes = "geom")
})

test_that("output has the correct number of vertices", {
  output <- geomRand(type = "polygon", vertices = 4)
  expect_data_frame(output@table, any.missing = FALSE, nrows = 4, ncols = 3)
})

test_that("Error if arguments have wrong value", {
  input <- matrix(nrow = 100, ncol = 100, data = 0)

  expect_error(geomRand(type = "bla"))
  expect_error(geomRand(template = "bla"))
  expect_error(geomRand(template = input, vertices = "bla"))
  expect_error(geomRand(template = input, show = "bla"))
})
