library(checkmate)
library(testthat)
context("loadData")


test_that("function loads 'csv' files", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  output <- loadData(files = c("aWindow.csv", "locations.csv"),
                     localPath = system.file("csv", package = "rasterTools"),
                     verbose = TRUE)
  expect_list(output, types = "geom")
})

test_that("function loads also from a 'catalog'", {
  abbr <- function(x){
    strsplit(x = x, split = "[.]")[[1]][1]
  }
  theFiles <- catalog(path = system.file("csv", package="rasterTools"), 
                      abbreviateBy = abbr, 
                      silent = TRUE)
  
  output <- loadData(files = theFiles,
                     localPath = system.file("csv", package = "rasterTools"))
  expect_list(output, types = "geom", len = 3)
  
})

test_that("function loads all in a directory", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  output <- loadData(localPath = system.file("csv", package = "rasterTools"))
  expect_list(output, types = "geom", len = 3)
})

test_that("function handles files that don't exist properly", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  output <- loadData(files = c("aWindow.csv", "anotherWindow.csv"),
                     localPath = system.file("csv", package = "rasterTools"))
  
  expect_character(output[[2]])
})

test_that("function loads 'kml' files", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  output <- loadData(files = "cgrs_estonia.kml",
                     localPath = system.file("test_datasets/kml", package="rasterTools"))
  expect_class(output, "geom")
})

test_that("function loads 'tif' files", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  output <- loadData(files = "g100_00.tif",
                     localPath = system.file("test_datasets/clc", package="rasterTools"))
  expect_class(output, "RasterLayer")
})

test_that("function loads 'hdf' files", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  loadData(files = "MOD44W.A2000001.h16v05.006.2018033153623.hdf",
                     localPath = system.file("test_datasets/modis/MOD44W", package="rasterTools"))
  expect_class(output, "RasterStack")
})

test_that("function loads 'svg' (emma) files", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  output <- loadData(files = "Ursus_arctos.svg",
                     layer = "emma",
                     localPath = system.file("test_datasets/emma", package="rasterTools"))
  expect_data_frame(output, any.missing = FALSE, ncols = 3)
  expect_names(names(output), must.include = c("species", "square", "year"))
  })

test_that("Error if arguments have wrong value", {
  expect_error(loadData())
  expect_error(loadData(dataset = 1))
  expect_error(loadData(files = list()))
  expect_error(loadData(localPath = 1))
  expect_error(loadData(files = "Ursus_arctos.svg",
                        layer = "bla",
                        localPath = system.file("test_datasets/emma", package="rasterTools")))
})
