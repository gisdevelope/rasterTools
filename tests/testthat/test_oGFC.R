library(checkmate)
library(testthat)
context("oGFC")


test_that("oGFC loads the correct file", {
  updatePaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- geomRectangle(data.frame(x = c(5094124, 5096249),
                                     y = c(4060501, 4061961))) %>%
    setCRS(crs = projs$laea)
  
  output <- oGFC(mask = myMask, years = 2000)
  expect_class(output, "RasterLayer")
})

test_that(("oGFC works with Spatial* mask (that has another crs than the dataset)"), {
  myMask <- geomRectangle(data.frame(x = c(5094124, 5096249),
                                     y = c(4060501, 4061961))) %>%
    setCRS(crs = projs$laea)
  myMask <- gToSp(geom = myMask) %>%
    setCRS(crs = projs$longlat)

  output <- oGFC(mask = myMask, years = 2000)
  expect_class(output, "RasterLayer")
})

test_that("Error if arguments have wrong value", {
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  expect_error(oGFC(mask = "myMask"))
  expect_error(oGFC(mask = myMask, years = "bla"))
  expect_error(oGFC(mask = myMask, years = 2012, keepRaw = "bla"))
})