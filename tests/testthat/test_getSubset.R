library(checkmate)
library(testthat)
library(raster)
context("getSubset")


test_that("getSubset of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = c(1, 1, 2, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  
  output <- getSubset(x = aGeom, subset = 1)
  expect_class(output, "geom")
  expect_true(dim(output@coords)[1] == 1)
  
  output <- getSubset(x = aGeom, subset = aGeom@coords$fid == 2)
  expect_class(output, "geom")
  expect_true(dim(output@coords)[1] == 2)
})
  