library(checkmate)
library(testthat)
context("oEMMA")


test_that("Error if arguments have wrong value", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  expect_error(oEMMA(mask = "myMask"))
  expect_error(oEMMA(mask = myMask, species = 1))
  expect_error(oEMMA(mask = myMask, species = "Apodemus flavicollis", version = "bla"))
  expect_error(oEMMA(mask = myMask, species = "Apodemus flavicollis", inclMeta = 1))
  
})