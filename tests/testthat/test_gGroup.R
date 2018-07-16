library(checkmate)
context("gGroup")


test_that("output is valid geometry", {
  input <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                      y = c(40, 40, 60, 70, 10, 20, 40),
                      id = 1)
  extent = data.frame(x = c(0, 80),
                      y = c(0, 80))
  aGeom <- geomPolygon(anchor = input, extent = extent)

  groupedGeom <- gGroup(geom = aGeom, distance = 40)
  expect_class(groupedGeom, classes = "geom")
  
  groupedGeom <- gGroup(geom = aGeom, index = c(1, 1, 1, 1, 2, 2, 2, 2))
  expect_class(groupedGeom, classes = "geom")

  groupedGeom <- gGroup(geom = aGeom, clusters = 2)
  expect_class(groupedGeom, classes = "geom")
})

test_that("output has different values (id) than input", {
  input <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                      y = c(40, 40, 60, 70, 10, 20, 40),
                      id = 1)
  extent = data.frame(x = c(0, 80),
                      y = c(0, 80))
  aGeom <- geomPolygon(anchor = input, extent = extent)
  
  groupedGeom <- gGroup(geom = aGeom, distance = 40)
  expect_false(all(aGeom@table[3] == groupedGeom@table[3]))
  
  groupedGeom <- gGroup(geom = aGeom, index = c(1, 1, 1, 1, 2, 2, 2, 2))
  expect_false(all(aGeom@table[3] == groupedGeom@table[3]))
  
  groupedGeom <- gGroup(geom = aGeom, clusters = 2)
  expect_false(all(aGeom@table[3] == groupedGeom@table[3]))
})

test_that("Error if arguments have wrong value", {
  input <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
                      y = c(40, 40, 60, 70, 10, 20, 40),
                      id = 1)
  extent = data.frame(x = c(0, 80),
                      y = c(0, 80))
  aGeom <- geomPolygon(anchor = input, extent = extent)

  expect_error(gGroup(geom = "bla"))
  expect_error(gGroup(geom = aGeom))
  expect_error(gGroup(geom = aGeom, distance = "bla"))
  expect_error(gGroup(geom = aGeom, index = "bla"))
  expect_error(gGroup(geom = aGeom, clusters = "bla"))
  expect_error(gGroup(geom = aGeom, clusters = 2.3))
})
