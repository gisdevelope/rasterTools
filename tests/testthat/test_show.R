library(checkmate)
library(testthat)
context("show")


test_that("show a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  output <- capture.output(show(object = aGeom))
  expect_true(length(output) == 8)
  expect_true(output[1] == "class        : geom")
})


