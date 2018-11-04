library(checkmate)
library(testthat)
library(magrittr)
context("oMODIS")


test_that("oMODIS loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- geomRectangle(data.frame(x = c(-2000000, -1900000),
                                     y = c(3800000, 4000000))) %>%
    setCRS(crs = projs$sinu)
  
  output <- oMODIS(mask = myMask, period = 2000, product = "MOD44W")
  expect_list(output, types = "RasterStack", len = 1)
})

test_that("Error if arguments have wrong value", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- geomRectangle(data.frame(x = c(-2000000, -1900000),
                                     y = c(3800000, 4000000))) %>%
    setCRS(crs = projs$sinu)
  
  expect_error(oMODIS(mask = "myMask"))
  expect_error(oMODIS(mask = myMask, period = "bla"))
  expect_error(oMODIS(mask = myMask, period = 2012, product = 1))
  expect_error(oMODIS(mask = myMask, period = c(2012000, 2017008), product = "MOD11A2"))
})
