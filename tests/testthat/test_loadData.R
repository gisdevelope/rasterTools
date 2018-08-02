library(checkmate)
library(testthat)
context("loadData")


test_that("function loads 'csv' files", {
  output <- loadData(files = c("aWindow.csv", "locations.csv"),
                     localPath = system.file("csv", package="rasterTools"),
                     verbose = TRUE)
  expect_class(output, "list")
  expect_class(output[[1]], "geom")
})

test_that("function loads also from a 'catalog'", {
  
})


# here come the 'load_*'-methods specific tests
test_that("function loads 'kml' files", {
  output <- loadData(files = "cgrs_estonia.kml",
                     localPath = system.file("kml", package="rasterTools"))
  expect_class(output, "SpatialPolygonsDataFrame")
})

test_that("function loads 'tif' files", {
  # output <- loadData(files = "cgrs_estonia.kml",
  #                    localPath = system.file("tif", package="rasterTools"))
  # expect_class(output, "raster")
})
