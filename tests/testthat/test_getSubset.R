library(checkmate)
library(testthat)
library(raster)
context("getSubset")


test_that("getSubset of a geom with a numeric", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = c(1, 2, 1, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPoint(anchor = coords, window = window)
  
  output <- getSubset(x = aGeom, subset = coords$id == 1)
  output2 <- getTable(output)
  expect_class(output, classes = "geom")
  expect_data_frame(output2, any.missing = FALSE, nrows = 2, ncols = 3)
  expect_names(names(output2), identical.to = c("x", "y", "id"))
})

test_that("getSubset of a geom with a logical", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = c(1, 2, 1, 2))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  
  output <- getSubset(x = aGeom, subse = c(FALSE, TRUE, FALSE, FALSE))
  output2 <- getTable(output)
  expect_data_frame(output2, any.missing = FALSE, nrows = 1, ncols = 3)
  expect_names(names(output2), identical.to = c("x", "y", "id"))
})