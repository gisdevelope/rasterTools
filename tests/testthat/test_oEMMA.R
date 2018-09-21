library(checkmate)
library(testthat)
context("oEMMA")


test_that("oEMMA loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)

  output <- oEMMA(mask = myMask, species = "Ursus arctos")
  expect_data_frame(output, any.missing = FALSE)
})

test_that(("oEMMA works with Spatial* mask (that has another crs than the dataset)"), {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  myMask <- gToSp(geom = myMask) %>%
    setCRS(crs = projs$longlat)

  output <- oEMMA(mask = myMask, species = "Ursus arctos")
  expect_data_frame(output, any.missing = FALSE)
})

test_that("Error if arguments have wrong value", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  expect_error(oEMMA(mask = "myMask"))
  expect_error(oEMMA(mask = myMask, species = 1))
  expect_error(oEMMA(mask = myMask, species = "Apodemus flavicollis", version = "bla"))
})

test_that("Warning if wrong species is specified", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  
  mySpecies <- tibble(original = c("Ursus arctos", "bla"), abbr = c("urs_arc", "bla"))
  expect_warning(oEMMA(mask = myMask, species = mySpecies))
})

test_that("bibliography item has been created", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- loadData(files = "aWindow.csv",
                     localPath = system.file("csv", package="rasterTools")) %>%
    geomRectangle() %>%
    setCRS(crs = projs$laea)
  options(bibliography = NULL)

  output <- oEMMA(mask = myMask, species = "Ursus arctos")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
})