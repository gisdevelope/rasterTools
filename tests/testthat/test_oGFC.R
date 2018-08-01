library(checkmate)
library(testthat)
context("oGFC")


test_that("Error if arguments have wrong value", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  expect_error(oGFC(mask = "myMask"))
  expect_error(oGFC(mask = myMask, years = "bla"))
  expect_error(oGFC(mask = myMask, years = 2012, keepRaw = "bla"))
  
})