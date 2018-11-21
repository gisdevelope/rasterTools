library(checkmate)
library(testthat)
context("oGFC")


test_that("oGFC loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- geomRectangle(data.frame(x = c(5094124, 5096249),
                                     y = c(4060501, 4061961))) %>%
    setCRS(crs = projs$laea)
  
  output <- oGFC(mask = myMask, years = 2000)
  expect_class(output, "RasterLayer")
})

test_that(("oGFC works with Spatial* mask (that has another crs than the dataset)"), {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- geomRectangle(data.frame(x = c(5094124, 5096249),
                                     y = c(4060501, 4061961))) %>%
    setCRS(crs = projs$laea)
  myMask <- gToSp(geom = myMask) %>%
    setCRS(crs = projs$longlat)

  output <- oGFC(mask = myMask, years = 2000)
  expect_class(output, "RasterLayer")
})

test_that("Error if arguments have wrong value", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  
  expect_error(oGFC(mask = "myMask"))
  expect_error(oGFC(mask = rtGeoms$mask, years = "bla"))
  expect_error(oGFC(mask = rtGeoms$mask, years = 2012, keepRaw = "bla"))
})

test_that("bibliography item has been created", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  options(bibliography = NULL)
  myMask <- geomRectangle(data.frame(x = c(5094124, 5096249),
                                     y = c(4060501, 4061961))) %>%
    setCRS(crs = projs$laea)
  
  output <- oGFC(mask = myMask, years = 2000)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
})
