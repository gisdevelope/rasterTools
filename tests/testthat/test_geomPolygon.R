library(checkmate)
library(testthat)
context("geomPolygon")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- geomPolygon(anchor = coords, window = window)
  expect_class(output, classes = "geom")
  expect_true(output@type == "polygon")
})

test_that("output has the correct number of vertices", {
  coords <- data.frame(x = c(40, 40),
                       y = c(40, 70),
                       fid = c(1))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- geomPolygon(anchor = coords, window = window, regular = TRUE, vertices = 6)
  expect_true(length(output@coords$fid) == 6)
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 40),
                       y = c(40, 70),
                       fid = c(1))

  expect_error(geomPolygon(anchor = "bla"))
  expect_error(geomPolygon(anchor = coords, window = "bla"))
  expect_error(geomPolygon(anchor = coords, vertices = "bla"))
  expect_error(geomPolygon(anchor = coords, regular = "bla"))
  expect_error(geomPolygon(anchor = coords, vertices = 4, regular = "bla"))
  expect_error(geomPolygon(anchor = coords, vertices = 4, regular = TRUE, show = "bla"))
  expect_error(geomPolygon(vertices = 4, regular = TRUE))
  expect_error(geomPolygon(template = "bla", vertices = 4))
})

