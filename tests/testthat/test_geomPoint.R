library(checkmate)
library(testthat)
context("geomPoint")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 40),
                       y = c(40, 70),
                       id = c(1, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- geomPoint(anchor = coords, window = window)
  expect_class(output, classes = "geom")
  expect_true(output@type == "point")
})

test_that("output has the correct number of vertices", {
  coords <- data.frame(x = c(40, 40),
                       y = c(40, 70),
                       id = c(1))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- geomPoint(anchor = coords, window = window)
  expect_true(length(output@table$id) == 2)
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  expect_error(geomPoint(anchor = "bla"))
  expect_error(geomPoint(anchor = coords, window = "bla"))
  expect_error(geomPoint(anchor = coords, vertices = "bla"))
  expect_error(geomPoint(template = "bla", vertices = 4))
})
