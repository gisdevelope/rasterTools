library(checkmate)
library(testthat)
library(magrittr)
library(raster)
context("oEFTA")


test_that("oEFTA loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))

  output <- oEFTA(mask = rtGeoms$mask, species = "Betula sp")
  expect_class(output, "RasterStack")
})

test_that(("oEFTA works with Spatial* mask (that has another crs than the dataset)"), {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- gToSp(geom = rtGeoms$mask) %>% 
    setCRS(crs = projs$longlat)
  
  output <- oEFTA(mask = myMask, species = "Betula sp")
  expect_class(output, "RasterStack")
})

test_that("Error if arguments have wrong value", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  
  expect_error(oEFTA(mask = "myMask"))
  expect_warning(oEFTA(mask = rtGeoms$mask, species = "bla"))
  expect_error(oEFTA(mask = rtGeoms$mask, type = 2001))
})

test_that("bibliography item has been created", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  options(bibliography = NULL)
  
  output <- oEFTA(mask = rtGeoms$mask, species = "Betula sp")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
})
