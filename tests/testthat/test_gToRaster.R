library(checkmate)
library(testthat)
library(raster)
context("gToRaster")


test_that("output has class RasterLayer", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, extent = extent)

  aRaster <- gToRaster(geom = aGeom)
  expect_class(aRaster, "RasterLayer")

  aRaster <- gToRaster(geom = aGeom, res = c(0.1, 0.1))
  expect_class(aRaster, "RasterLayer")
})

test_that("output has proper coordinate reference system", {
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                            5234735, 5281527, 5189955, 5041066),
                      Y = c(3977612, 3971119, 4028167, 3997230,
                            4060164, 4117856, 4118207, 4062838),
                      fid = c(1, 1, 2, 2, 2, 2, 1, 1))
  aGeom <- geomPolygon(anchor = input)
  aRaster <- gToRaster(geom = aGeom, crs = projs$laea, res = c(1000, 1000))
  expect_equal(aRaster@crs@projargs, projs$laea)

  aGeom <- geomPolygon(anchor = input)
  aGeom <- setCRS(x = aGeom, crs = projs$laea)
  aRaster <- gToRaster(geom = aGeom, crs = projs$laea, res = c(1000, 1000))
  expect_equal(aRaster@crs@projargs, projs$laea)
})

test_that("output is correct resolution", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, extent = extent)
  
  aRaster <- gToRaster(geom = aGeom)
  expect_equal(res(aRaster), c(1, 1))
  
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                            5234735, 5281527, 5189955, 5041066),
                      Y = c(3977612, 3971119, 4028167, 3997230,
                            4060164, 4117856, 4118207, 4062838),
                      fid = c(1, 1, 2, 2, 2, 2, 1, 1))
  aGeom <- geomPolygon(anchor = coords, extent = extent)
  aRaster <- gToRaster(geom = aGeom, crs = projs$laea, res = c(100, 100))
  expect_equal(res(aRaster), c(100, 100))
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, extent = extent)

  expect_error(gToRaster(geom = "bla"))
  expect_error(gToRaster(geom = notAGeom))
  expect_error(gToRaster(geom = aGeom, negative = "bla"))
  expect_error(gToRaster(geom = aGeom, crs = "LAEA"))
})

