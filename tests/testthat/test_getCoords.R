library(checkmate)
library(testthat)
library(raster)
context("getCoords")


test_that("getCoords of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  output <- getCoords(aGeom)
  
  expect_data_frame(output, any.missing = FALSE, nrows = 4, ncols = 4)
  expect_names(names(output), identical.to = c("id", "fid", "x", "y"))
})
