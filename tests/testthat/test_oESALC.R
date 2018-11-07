library(checkmate)
library(testthat)
library(magrittr)
context("oESALC")


test_that("oESALC loads the correct file", {
  skip_on_appveyor()
  setPaths(root = system.file("test_datasets", package="rasterTools"))

  output <- oESALC(mask = rtGeoms$mask)
  expect_list(output, types = "RasterStack", len = 1)
})

test_that("Error if arguments have wrong value", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- geomRectangle(data.frame(x = c(-2000000, -1900000),
                                     y = c(3800000, 4000000))) %>%
    setCRS(crs = projs$sinu)
  
  # expect_error(oESALC(mask = "myMask"))
  # expect_error(oESALC(mask = myMask, ))
  # expect_error(oESALC(mask = myMask, )
  # expect_error(oESALC(mask = myMask, ))
})
