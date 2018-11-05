library(checkmate)
library(testthat)
library(magrittr)
library(raster)
context("oCLC")


test_that("oCLC loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  
  output <- oCLC(mask = rtGeoms$mask, years = 2000)
  expect_class(output, "RasterStack")
})

test_that(("oCLC works with Spatial* mask (that has another crs than the dataset)"), {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- gToSp(geom = rtGeoms$mask) %>% 
    setCRS(crs = projs$longlat)
  
  output <- oCLC(mask = myMask, years = 2000)
  expect_class(output, "RasterStack")
})

test_that("Error if arguments have wrong value", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  
  expect_error(oCLC(mask = "myMask"))
  expect_error(oCLC(mask = rtGeoms$mask, years = "bla"))
  expect_error(oCLC(mask = rtGeoms$mask, years = 2001))
})

test_that("bibliography item has been created", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  options(bibliography = NULL)
  
  output <- oCLC(mask = rtGeoms$mask, years = 2000)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
})