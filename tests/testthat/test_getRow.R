library(checkmate)
library(testthat)
library(raster)
context("getRow")


test_that("getRow of a geom with a numeric", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  output <- getRow(x = aGeom, row = c(1:2))
  
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 3)
  expect_names(names(output), identical.to = c("x", "y", "id"))
})

test_that("getRow of a geom with a logical", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  
  output <- getRow(x = aGeom, row = c(FALSE, TRUE, FALSE, FALSE))
  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 3)
})