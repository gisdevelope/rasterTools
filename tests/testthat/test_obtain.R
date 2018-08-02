library(checkmate)
library(testthat)
library(magrittr)
context("obtain")


test_that("obtain works", {
  updatePaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  myDatasets <- list(list(operator = "oCLC", years = 2000))
  
  output <- obtain(data = myDatasets, mask = myMask)
  expect_list(output, len = 1)
  expect_names(names(output), must.include = c("clc"))
})

test_that("Warning if operator does not exist", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea) %>% 
    gToSp(crs = projs$laea)
  myDatasets <- list(list(operator = "oCLC", years = 2000),
                     list(operator = "oBLA", something = 1),
                     list(operator = "oGFC", years = 2005))
  
  output <- obtain(data = myDatasets, mask = myMask)
  expect_list(output, len = 2)
  expect_names(names(output), must.include = c("clc", "gfc"))
})

test_that("Error if arguments have wrong value", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  myDatasets <- list(list(operator = "oCLC", years = 2000))
  
  expect_error(obtain(data = "myDatasets", mask = myMask))
  expect_error(obtain(data = myDatasets, mask = "myMask"))
})