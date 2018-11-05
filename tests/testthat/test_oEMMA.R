library(checkmate)
library(testthat)
context("oEMMA")


test_that("oEMMA loads the correct file", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))

  output <- oEMMA(mask = rtGeoms$mask, species = "Ursus arctos")
  expect_data_frame(output, any.missing = FALSE)
})

test_that(("oEMMA works with Spatial* mask (that has another crs than the dataset)"), {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  myMask <- gToSp(geom = rtGeoms$mask) %>%
    setCRS(crs = projs$longlat)

  output <- oEMMA(mask = myMask, species = "Ursus arctos")
  expect_data_frame(output, any.missing = FALSE)
})

test_that("Error if arguments have wrong value", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  
  expect_error(oEMMA(mask = "myMask"))
  expect_error(oEMMA(mask = rtGeoms$mask, species = 1))
  expect_error(oEMMA(mask = rtGeoms$mask, species = "Apodemus flavicollis", version = "bla"))
})

test_that("Warning if wrong species is specified", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))
  
  mySpecies <- tibble(original = c("Ursus arctos", "bla"), abbr = c("urs_arc", "bla"))
  expect_warning(oEMMA(mask = rtGeoms$mask, species = mySpecies))
})

test_that("bibliography item has been created", {
  setPaths(root = system.file("test_datasets", package="rasterTools"))

  options(bibliography = NULL)

  output <- oEMMA(mask = rtGeoms$mask, species = "Ursus arctos")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
})