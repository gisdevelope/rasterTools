library(checkmate)
library(testthat)
library(raster)
context("getColumn")


test_that("getColumn of a geom with a numeric", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  
  output <- getColumn(x = aGeom, column = 2)
  expect_vector(output, any.missing = FALSE, len = 4)
})

test_that("getColumn of a geom with a logical", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  
  output <- getColumn(x = aGeom, column = c(TRUE, FALSE, FALSE))
  expect_vector(output, any.missing = FALSE, len = 4)
})

test_that("getColumn of a geom with a character", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  
  output <- getColumn(x = aGeom, column = "x")
  expect_vector(output, any.missing = FALSE, len = 4)
})
