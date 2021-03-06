library(checkmate)
library(testthat)
library(raster)
context("setTable")


test_that("setTable of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  input <- geomPolygon(anchor = coords, window = window)
  attributes <- data.frame(fid = 1, variable = "A")
  
  output <- setTable(input, attributes)
  expect_class(output, "geom")
  expect_data_frame(output@attr, ncols = 3)
  expect_names(names(output@attr), must.include = c("fid", "n", "variable"))
})

test_that("setTable of a 'RasterLayer'", {
  input <- rtData$continuous
  attributes <- data.frame(id = 1:91, variable = rep(LETTERS, length.out = 91))
  
  # test RasterLayer without attribute table
  output <- setTable(input, attributes)
  expect_class(output, "RasterLayer")
  expect_true(output@data@isfactor)
})

