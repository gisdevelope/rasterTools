library(checkmate)
library(testthat)
library(raster)
context("getHistory")


test_that("getHistory of a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  output <- getHistory(aGeom)
  
  expect_list(output, any.missing = FALSE, types = "character")
  expect_true(output[[1]] == "geometry was created as 'polygon'")
})

test_that("getHistory of a Raster*", {
  continuous <- rtData$continuous
  patches <- rPatches(rBinarise(continuous, thresh = 40))
  anAlgo <- list(background = list(operator = "rBinarise", thresh = 30),
                 background = list(operator = "rPatches"),
                 background = list(operator = "rSegregate", background = 0),
                 background = list(operator = "rBinarise", thresh = 1))
  segregated <- modify(input = continuous, by = anAlgo)

  output <- getHistory(segregated)
  expect_list(output, len = 140)
  
  output <- getHistory(patches)
  expect_list(output, len = 3)
})
