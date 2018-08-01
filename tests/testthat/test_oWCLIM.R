library(checkmate)
context("oWCLIM")


test_that("Error if arguments have wrong value", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  expect_error(oWCLIM(mask = "myMask"))
  expect_error(oWCLIM(mask = myMask, variable = 1))
  expect_error(oWCLIM(mask = myMask, month = "bla"))
  expect_error(oWCLIM(mask = myMask, resolution = "bla"))
  expect_error(oWCLIM(mask = myMask, version = "bla"))
  
})