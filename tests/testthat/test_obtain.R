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
    setCRS(crs = projs$laea)
  myDatasets <- list(list(operator = "oCLC", years = 2000),
                     list(operator = "oBLA", something = 1))
  
  output <- obtain(data = myDatasets, mask = myMask)
  expect_list(output, len = 1)
  expect_names(names(output), must.include = c("clc"))
})