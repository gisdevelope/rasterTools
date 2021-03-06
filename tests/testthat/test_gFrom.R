library(checkmate)
library(testthat)
library(sp)
library(sf)
context("gFrom")


test_that("transform from 'Spatial*", {
  
  # test 'SpatialPoints'
  input <- SpatialPoints(data.frame(c(1,2,3,4,5), 
                                    c(3,2,5,1,4)))
  
  output <- gFrom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
  
  # test 'SpatialPointsDataFrame'
  pts = cbind(1:5, 1:5)
  dimnames(pts)[[1]] = letters[1:5]
  df = data.frame(a = 1:5)
  row.names(df) = letters[5:1]
  input <- SpatialPointsDataFrame(pts, df, match.ID = TRUE)
  
  output <- gFrom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
  
  # test 'SpatialMultiPoints'
  cl1 <- cbind(rnorm(3, 10), rnorm(3, 10))
  cl2 <- cbind(rnorm(5, 10), rnorm(5,  0))
  cl3 <- cbind(rnorm(7,  0), rnorm(7, 10))
  input <- SpatialMultiPoints(list(cl1, cl2, cl3))
  
  output <- gFrom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
  expect_true(length(unique(output@coords$fid)) == 3)
  
  # test 'SpatialMultiPointsDataFrame'
  input <- SpatialMultiPointsDataFrame(list(cl1, cl2, cl3), data = data.frame(a = 1:3))
  
  output <- gFrom(input)
  expect_class(output, "geom")
  expect_true(output@type == "point")
  expect_data_frame(getTable(output), nrows = 3, ncols = 3)
  
  # test 'SpatialLines'
  input <- SpatialLines(list(Lines(list(Line(cbind(c(1, 2, 3), c(3, 2, 2)))), ID = "a"), 
                             Lines(list(Line(cbind(c(1, 2, 3), c(1, 1.5, 1)))), ID = "b")))
  
  output <- gFrom(input)
  expect_class(output, "geom")
  expect_true(output@type == "line")
  expect_true(length(unique(output@coords$fid)) == 2)
  
  # test 'SpatialLinesDataFrame'
  input <- SpatialLinesDataFrame(input, data = data.frame(a = 1:2), match.ID = FALSE)
  
  output <- gFrom(input)
  expect_class(output, "geom")
  expect_true(output@type == "line")
  expect_data_frame(getTable(output), nrows = 2, ncols = 3)
  
  # test 'SpatialPolygons'
  Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
  Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
  Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
  Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
  
  Srs1 = Polygons(list(Sr1), "s1")
  Srs2 = Polygons(list(Sr2), "s2")
  Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
  input = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
  
  output <- gFrom(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")

  # test 'SpatialPolygonsDataFrame'
  input <- SpatialPolygonsDataFrame(input, data = data.frame(a = 1:3), match.ID = FALSE)
  
  output <- gFrom(input)
  expect_class(output, "geom")
  expect_true(output@type == "polygon")
  expect_data_frame(getTable(output), nrows = 4, ncols = 3)

  # test 'SpatialGrid'
  
  # output <- gFrom(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "")
  
  # test 'SpatialGridDataFrame'
  
  # output <- gFrom(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "")
  
  # test 'SpatialPixels'
  
  # output <- gFrom(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "")
  
  # test 'SpatialPixelsDataFrame'
  
  # output <- gFrom(input)
  # expect_class(output, "geom")
  # expect_true(output@type == "")
})

test_that("transform from sf", {
  # test POINT
  pt <- st_sfc(st_point(x = c(1, 2)))
  input <- data.frame(bla = 1)
  st_geometry(input) <- pt
  
  output <- gFrom(input)
  
  # test MULTIPOINT
  pts = matrix(data = 1:10, ncol = 2)
  mp <- st_sfc(st_multipoint(pts))
  input <- data.frame(bla = 1)
  st_geometry(input) <- mp
  
  output <- gFrom(input)
  
  # test LINESTRING
  pts = matrix(1:10, ncol = 2)
  ls = st_sfc(st_linestring(pts))
  input <- data.frame(bla = 1)
  st_geometry(input) <- ls
  
  output <- gFrom(input)
  
  # test MULTILINESTRING
  outer <- matrix(c(0,0,10,0,10,10,0,10,0,0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1,1,1,2,2,2,2,1,1,1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5,5,5,6,6,6,6,5,5,5), ncol = 2, byrow = TRUE)
  pts <- list(outer, hole1, hole2)
  ml <- st_sfc(st_multilinestring(pts))
  input <- data.frame(bla = 1)
  st_geometry(input) <- ml
  
  
  output <- gFrom(input)
  
  # test POLYGON
  outer <- matrix(c(0,0,10,0,10,10,0,10,0,0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1,1,1,2,2,2,2,1,1,1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5,5,5,6,6,6,6,5,5,5), ncol = 2, byrow = TRUE)
  pts <- list(outer, hole1, hole2)
  pg <- st_sfc(st_polygon(pts))
  input <- data.frame(bla = 1)
  st_geometry(input) <- pg
  
  output <- gFrom(input)
  
  # test MULTIPOLYGON
  pol1 <- list(outer, hole1, hole2)
  pol2 <- list(outer + 12, hole1 + 12)
  pol3 <- list(outer + 24)
  mp <- list(pol1,pol2,pol3)
  mp <- st_sfc(st_multipolygon(mp))
  input <- data.frame(bla = 1)
  st_geometry(input) <- pg
  
  output <- gFrom(input)
  
})

test_that("output has coordinate reference system, if set", {
  # input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
  #                           5234735, 5281527, 5189955, 5041066),
  #                     Y = c(3977612, 3971119, 4028167, 3997230,
  #                           4060164, 4117856, 4118207, 4062838),
  #                     id = c(1, 1, 2, 2, 2, 2, 1, 1))
  # polyGeom <- geomPolygon(anchor = input, show = FALSE)
  # spPolygon <- gToSp(geom = polyGeom, crs = projs$laea)
  # expect_equal(spPolygon@proj4string@projargs, projs$laea)
})

test_that("Error if arguments have wrong value", {
  # notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
  #                        y = c(15, 25, 20, 40, 45))
  # input <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
  #                           5234735, 5281527, 5189955, 5041066),
  #                     Y = c(3977612, 3971119, 4028167, 3997230,
  #                           4060164, 4117856, 4118207, 4062838),
  #                     id = c(1, 1, 2, 2, 2, 2, 1, 1))
  # aGeom <- geomPolygon(anchor = input, show = FALSE)
  #
  # expect_error(gToSp(geom = "bla"))
  # expect_error(gToSp(geom = notAGeom))
  # expect_error(gToSp(geom = aGeom, crs = "LAEA"))
})

test_that("output is valid geometry", {
  # coords <- data.frame(x = c(40, 70, 70, 50),
  #                      y = c(40, 40, 60, 70),
  #                      id = 1)
  # window <- data.frame(x = c(0, 80),
  #                      y = c(0, 80))
  #
  # output <- geomPolygon(anchor = coords, window = window)
  # expect_class(output, classes = "geom")
  # expect_true(output@type == "polygon")
})

test_that("output has the correct number of vertices", {
  # coords <- data.frame(x = c(40, 40),
  #                      y = c(40, 70),
  #                      id = c(1))
  # window <- data.frame(x = c(0, 80),
  #                      y = c(0, 80))
  #
  # output <- geomPolygon(anchor = coords, window = window, regular = TRUE, vertices = 6)
  # expect_true(length(output@table$id) == 6)
})

test_that("Error if arguments have wrong value", {
  # coords <- data.frame(x = c(40, 40),
  #                      y = c(40, 70),
  #                      id = c(1))
  #
  # expect_error(geomPolygon(anchor = "bla"))
  # expect_error(geomPolygon(anchor = coords, window = "bla"))
  # expect_error(geomPolygon(anchor = coords, vertices = "bla"))
  # expect_error(geomPolygon(anchor = coords, regular = "bla"))
  # expect_error(geomPolygon(anchor = coords, vertices = 4, regular = "bla"))
  # expect_error(geomPolygon(anchor = coords, vertices = 4, regular = TRUE, show = "bla"))
  # expect_error(geomPolygon(vertices = 4, regular = TRUE))
  # expect_error(geomPolygon(template = "bla", vertices = 4))
})
