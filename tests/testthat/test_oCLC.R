library(checkmate)
library(testthat)
library(magrittr)
library(raster)
context("oCLC")


test_that("oCLC loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  output <- oCLC(mask = myMask, years = 2000)
  expect_class(output, "RasterStack")
})

test_that(("oCLC works with Spatial* mask (that has another crs than the dataset)"), {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  myMask <- gToSp(geom = myMask) %>% 
    setCRS(crs = projs$longlat)
  
  output <- oCLC(mask = myMask, years = 2000)
  expect_class(output, "RasterStack")
})

test_that("Error if arguments have wrong value", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  expect_error(oCLC(mask = "myMask"))
  expect_error(oCLC(mask = myMask, years = "bla"))
  expect_error(oCLC(mask = myMask, years = 2001))
})

test_that("bibliography item has been created", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  options(bibliography = NULL)
  
  output <- oCLC(mask = myMask, years = 2000)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
})