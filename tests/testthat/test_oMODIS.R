library(checkmate)
context("oMODIS")


test_that("Error if arguments have wrong value", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  expect_error(oMODIS(mask = "myMask"))
  expect_error(oMODIS(mask = myMask, period = "bla"))
  expect_error(oMODIS(mask = myMask, period = 2012, product = 1))
  expect_error(oMODIS(mask = myMask, period = 2012, product = "MOD11A2"))
  expect_error(oMODIS(mask = myMask, period = 2012, product = "MOD11A2", raw = 1))
  
})