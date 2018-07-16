library(checkmate)
library(testthat)
library(raster)
library(sp)
context("setCRS")


test_that("setCRS of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  output <- setCRS(x = aGeom, crs = projs$laea)
  test <- getCRS(output)

  expect_class(output, classes = "geom")
  expect_true(projs$laea == test)
})

test_that("getExtent of a Raster", {
  aRaster <- raster(xmn=-110, xmx=-90, ymn=40, ymx=60, ncols=40, nrows=40)
  aRaster <- setValues(aRaster, 1:ncell(aRaster))
  theCRS <- projection(aRaster)
  crs(aRaster) <- as.character(NA)
  
  # test when crs is missing
  output <- setCRS(x = aRaster, crs = theCRS)
  expect_class(crs(output), classes = "CRS")
  expect_character(crs(output)@projargs, any.missing = FALSE, pattern = "+proj=longlat", len = 1)
  
  # test to reproject an existing crs
  output2 <- setCRS(x = output, crs = projs$laea)
  expect_class(crs(output2), classes = "CRS")
  expect_character(crs(output2)@projargs, any.missing = FALSE, pattern = "+proj=laea", len = 1)
})

test_that("getExtent of a Spatial", {
  x = c(1, 2, 3, 4, 5)
  y = c(3, 2, 5, 1, 4)
  aSpatial <- SpatialPoints(cbind(x, y))

  output <- setCRS(x = aSpatial, crs = projs$laea)
  expect_class(output, classes = "SpatialPoints")
  expect_character(proj4string(output), any.missing = FALSE, pattern = "+proj=laea", len = 1)
})