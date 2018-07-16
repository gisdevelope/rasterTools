library(checkmate)
library(testthat)
library(raster)
context("getCRS")


test_that("getCRS of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  aGeom <- setCRS(x = aGeom, crs = projs$laea)
  output <- getCRS(aGeom)
  
  expect_character(output, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})

test_that("getExtent of a Raster", {
  aRaster <- raster(nrows=108, ncols=21, xmn=0, xmx=10)
  
  output <- getCRS(aRaster)
  expect_character(output, any.missing = FALSE, pattern = "+proj=longlat", len = 1)
})

test_that("getExtent of a Spatial", {
  x = c(1, 2, 3, 4, 5)
  y = c(3, 2, 5, 1, 4)
  aSpatial <- SpatialPoints(cbind(x, y))
  proj4string(aSpatial) <- projs$laea
  
  output <- getCRS(aSpatial)
  expect_character(output, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})