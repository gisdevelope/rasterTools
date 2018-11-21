library(checkmate)
library(testthat)
library(magrittr)
context("oESALC")


test_that("oESALC loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  
  output <- oESALC(mask = rtGeoms$mask, years = 2005)
  
  expect_class(output, "RasterStack")
})

test_that(("oESALC works with Spatial* mask (that has another crs than the dataset)"), {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- gToSp(geom = rtGeoms$mask) %>%
    setCRS(crs = projs$longlat)
  
  output <- oESALC(mask = myMask, years = 2005)
  expect_class(output, "RasterStack")
})

test_that("Error if arguments have wrong value", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  
  expect_error(oESALC(mask = "myMask"))
  expect_error(oESALC(mask = rtGeoms$mask, years = "bla"))
  expect_error(oESALC(mask = rtGeoms$mask, years = 1991))
})

test_that("bibliography item has been created", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  options(bibliography = NULL)
  
  output <- oESALC(mask = rtGeoms$mask, years = 2005)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
})
