library(checkmate)
library(magrittr)
context("oCLC")


test_that("Error if arguments have wrong value", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
 expect_error(oCLC(mask = myMask))
 expect_error(oCLC(mask = myMask, years = "bla"))
 
})