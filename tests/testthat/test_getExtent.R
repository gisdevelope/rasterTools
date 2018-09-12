library(checkmate)
library(testthat)
library(raster)
library(sp)
context("getExtent")


test_that("getExtent of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  
  output <- getExtent(aGeom)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getExtent of a Raster", {
  aRaster <- raster(nrows=108, ncols=21, xmn=0, xmx=10)
  
  output <- getExtent(aRaster)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getExtent of a Spatial", {
  x = c(1, 2, 3, 4, 5)
  y = c(3, 2, 5, 1, 4)
  aSpatial <- SpatialPoints(cbind(x, y))
  
  output <- getExtent(aSpatial)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

test_that("getExtent of a matrix", {
  aMatrix <- matrix(ncol = 100, nrow = 100, data = 5)
  
  output <- getExtent(aMatrix)
  expect_data_frame(output, any.missing = FALSE, nrows = 2, ncols = 2)
  expect_names(names(output), identical.to = c("x", "y"))
})

