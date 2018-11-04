library(checkmate)
library(testthat)
library(magrittr)
context("obtain")


test_that("obtain works", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myDatasets <- list(list(operator = "oCLC", years = 2000))
  
  output <- obtain(data = myDatasets, mask = rtGeoms$mask)
  expect_list(output, len = 1)
  expect_names(names(output), must.include = c("clc"))
})

test_that("obtain works also on a list of 'mask'", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- rtGeoms$mask %>% 
    gGroup(index = c(1, 2, 1, 2))
  myDatasets <- list(list(operator = "oCLC", years = 2000))
  
  output <- obtain(data = myDatasets, mask = myMask)
  expect_list(output, len = 2)
  expect_names(names(output), must.include = c("mask_2", "mask_1"))
})

test_that("obtain works also on a Spatial* 'mask'", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  rtGeoms$mask %>% 
    gToSp()
  myDatasets <- list(list(operator = "oCLC", years = 2000))
  
  output <- obtain(data = myDatasets, mask = rtGeoms$mask)
  expect_list(output, len = 1)
  expect_names(names(output), must.include = c("clc"))
})

test_that("Warning if operator does not exist", {
  myDatasets <- list(list(operator = "oCLC", years = 2000),
                     list(operator = "oBLA", something = 1),
                     list(operator = "oGFC", years = 2005))
  
  output <- obtain(data = myDatasets, mask = rtGeoms$mask)
  expect_list(output, len = 2)
  expect_names(names(output), must.include = c("clc", "gfc"))
})

test_that("Error if arguments have wrong value", {
  myDatasets <- list(list(operator = "oCLC", years = 2000))
  
  expect_error(obtain(data = "myDatasets", mask = rtGeoms$mask))
  expect_error(obtain(data = myDatasets, mask = "rtGeoms$mask"))
})
