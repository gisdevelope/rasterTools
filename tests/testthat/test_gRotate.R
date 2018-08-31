library(checkmate)
context("gRotate")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, extent = extent)
  rotGeom <- gRotate(geom = aGeom, angle = 45, about = c(50, 30))
  rotGeom2 <- gRotate(geom = aGeom, angle = -45)
  
  expect_class(rotGeom, classes = "geom")
  
  expect_class(rotGeom2, classes = "geom")
})

test_that("output has different coordinates than input", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, extent = extent)
  rotGeom <- gRotate(geom = aGeom, angle = 45, about = c(50, 30))
  
  expect_false(all(getCoords(aGeom)[c(3, 4)] == getCoords(rotGeom)[c(3, 4)]))
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, extent = extent)
  
  expect_error(gRotate(geom = "bla"))
  expect_error(gRotate(geom = aGeom))
  expect_error(gRotate(geom = aGeom, angle = "bla"))
  expect_error(gRotate(geom = notAGeom, angle = 45))
  expect_error(gRotate(geom = aGeom, angle = 45, about = "bla"))
})