library(checkmate)
library(testthat)
library(magrittr)
context("oWCLIM")


test_that("oWCLIM loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- geomRectangle(data.frame(x = c(5093616, 5103222),
                                     y = c(4054188, 4064870))) %>%
    setCRS(crs = projs$laea)
  
  output <- oWCLIM(mask = myMask, variable = c("tmean"), month = 5)
  expect_list(output, len = 1)
  expect_class(output$tavg, "RasterStack")
  
  output <- oWCLIM(mask = myMask, variable = c("tmean"), month = 2, version = 1.4)
  expect_list(output, len = 1)
  expect_class(output$tmean, "RasterStack")
})

test_that(("oWCLIM works with Spatial* mask (that has another crs than the dataset)"), {
  myMask <- geomRectangle(data.frame(x = c(5093616, 5103222),
                                     y = c(4054188, 4064870))) %>%
    setCRS(crs = projs$laea)
  myMask <- gToSp(geom = myMask) %>%
    setCRS(crs = projs$longlat)

  output <- oWCLIM(mask = myMask, variable = c("tmean"), month = 5)
  expect_list(output, len = 1)
  expect_class(output$tavg, "RasterStack")
})

test_that("Error if arguments have wrong value", {
  myMask <- geomRectangle(data.frame(x = c(5093616, 5103222),
                                     y = c(4054188, 4064870))) %>%
    setCRS(crs = projs$laea)

  expect_error(oWCLIM(mask = "myMask"))
  expect_error(oWCLIM(mask = myMask, variable = 1))
  expect_error(oWCLIM(mask = myMask, variable = "tmin", month = "bla"))
  expect_error(oWCLIM(mask = myMask, variable = "tmin", resolution = "bla"))
  expect_error(oWCLIM(mask = myMask, variable = "tmin", version = "bla"))
})

test_that("bibliography item has been created", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- geomRectangle(data.frame(x = c(5093616, 5103222),
                                     y = c(4054188, 4064870))) %>%
    setCRS(crs = projs$laea)
  
  output <- oWCLIM(mask = myMask, variable = c("tmean"), month = 5)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})
