library(checkmate)
library(testthat)
library(magrittr)
context("oMODIS")


test_that("oMODIS loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  # output <- oMODIS(mask = myMask, period = c(2017001, 2017008), product = "MOD11A2")
  # expect_class(output, "RasterLayer")
})

test_that("Error if arguments have wrong value", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  expect_error(oMODIS(mask = "myMask"))
  expect_error(oMODIS(mask = myMask, period = "bla"))
  expect_error(oMODIS(mask = myMask, period = 2012, product = 1))
  expect_error(oMODIS(mask = myMask, period = c(2012000, 2017008), product = "MOD11A2"))
})
