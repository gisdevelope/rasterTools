library(checkmate)
library(testthat)
context("gToSp")


test_that("output has class Spatial*", {
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                           5234735, 5281527, 5189955, 5041066),
                           Y = c(3977612, 3971119, 4028167, 3997230,
                           4060164, 4117856, 4118207, 4062838),
                           fid = c(1:8))

  pointsGeom <- geomPoint(anchor = input)
  spPoints <- gToSp(geom = pointsGeom, crs = projs$laea)
  expect_class(spPoints, "SpatialPoints")

  # linesGeom <- geomCurve(anchor = somePoints, show = FALSE)
  # spLines <- gToSp(geom = linesGeom, crs = LAEA)
  # expect_class(spPoints, "SpatialLines")

  pointsGeom <- gGroup(geom = pointsGeom, index = c(rep(1, 8)))
  polyGeom <- geomPolygon(anchor = pointsGeom)
  spPolygon <- gToSp(geom = polyGeom, crs = projs$laea)
  expect_class(spPolygon, "SpatialPolygons")
})

test_that("output has correct length", {
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                            5234735, 5281527, 5189955, 5041066),
                      Y = c(3977612, 3971119, 4028167, 3997230,
                            4060164, 4117856, 4118207, 4062838),
                      fid = c(1, 1, 2, 2, 2, 2, 1, 1))
  polyGeom <- geomPolygon(anchor = input)
  spPolygon <- gToSp(geom = polyGeom, crs = projs$laea)
  expect_equal(length(spPolygon), 2)
})

test_that("output has proper coordinate reference system", {
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                            5234735, 5281527, 5189955, 5041066),
                      Y = c(3977612, 3971119, 4028167, 3997230,
                            4060164, 4117856, 4118207, 4062838),
                      fid = c(1, 1, 2, 2, 2, 2, 1, 1))
  polyGeom <- geomPolygon(anchor = input, show = FALSE)
  spPolygon <- gToSp(geom = polyGeom, crs = projs$laea)
  expect_equal(spPolygon@proj4string@projargs, projs$laea)
  
  spPolygon <- gToSp(geom = polyGeom)
  expect_equal(spPolygon@proj4string@projargs, as.character(NA))
  
  polyGeom <- setCRS(x = polyGeom, crs = projs$laea)
  spPolygon <- gToSp(geom = polyGeom, crs = projs$longlat)
  expect_equal(spPolygon@proj4string@projargs, projs$longlat)
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))
  input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
                            5234735, 5281527, 5189955, 5041066),
                      Y = c(3977612, 3971119, 4028167, 3997230,
                            4060164, 4117856, 4118207, 4062838),
                      id = c(1, 1, 2, 2, 2, 2, 1, 1))
  aGeom <- geomPolygon(anchor = input, show = FALSE)

  expect_error(gToSp(geom = "bla"))
  expect_error(gToSp(geom = notAGeom))
  expect_error(gToSp(geom = aGeom, crs = "LAEA"))
})
