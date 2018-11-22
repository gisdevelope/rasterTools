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
  expect_class(output, classes = "RasterStack")
})

test_that(("oMODIS works with Spatial* mask (that has another crs than the dataset)"), {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- geomRectangle(data.frame(x = c(-2000000, -1900000),
                                     y = c(3800000, 4000000))) %>%
    setCRS(crs = projs$sinu) %>% 
    setCRS(crs = projs$laea)
  
  output <- oMODIS(mask = myMask, period = 2000, product = "MOD44W")
  expect_class(output, "RasterStack")
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

test_that("bibliography item has been created", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  options(bibliography = NULL)
  myMask <- geomRectangle(data.frame(x = c(-2000000, -1900000),
                                     y = c(3800000, 4000000))) %>%
    setCRS(crs = projs$sinu)
  
  output <- oMODIS(mask = myMask, period = 2000, product = "MOD44W")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
})
