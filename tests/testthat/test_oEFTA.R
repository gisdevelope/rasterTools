library(checkmate)
library(testthat)
library(magrittr)
library(raster)
context("oEFTA")


test_that("oEFTA loads the correct file", {
  updatePaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aSmallWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)

  output <- oEFTA(mask = myMask, species = "Betula sp")
  expect_class(output, "RasterStack")
})

test_that(("oEFTA works with Spatial* mask (that has another crs than the dataset)"), {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  myMask <- gToSp(geom = myMask) %>% 
    setCRS(crs = projs$longlat)
  
  output <- oEFTA(mask = myMask, species = "Betula sp")
  expect_class(output, "RasterStack")
})

test_that("Error if arguments have wrong value", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  expect_error(oEFTA(mask = "myMask"))
  expect_warning(oEFTA(mask = myMask, species = "bla"))
  expect_error(oEFTA(mask = myMask, type = 2001))
})

test_that("bibliography item has been created", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  options(bibliography = NULL)
  
  output <- oEFTA(mask = myMask, species = "Betula sp")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
})
