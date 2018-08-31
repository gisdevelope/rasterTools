library(checkmate)
library(raster)
context("visualise")


test_that("visualise a Raster* object", {
  continuous <- rtData$continuous
  
  output <- visualise(raster = continuous)
  expect_class(output, "recordedplot")
})

test_that("visualise a matrix", {
  continuous <- raster::as.matrix(rtData$continuous)
  
  output <- visualise(raster = continuous)
  expect_class(output, "recordedplot")
})

test_that("visualise an image", {
  continuous <- rtData$continuous
  input <- RGB(continuous)
  
  output <- visualise(raster = input, image = TRUE)
  expect_class(output, "recordedplot")
})

test_that("visualise a geom", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  input <- geomPolygon(anchor = coords)
  
  output <- visualise(geom = input)
  expect_class(output, "recordedplot")
})

test_that("visualise an object with NA values", {
  continuous <- rtData$continuous
  get_patches <- list(list(operator = "rBinarise", thresh = 30),
                      list(operator = "rPatches"))
  myPatches <- modify(input = continuous, by = get_patches, sequential = TRUE)
  
  output <- visualise(raster = myPatches)
  expect_class(output, "recordedplot")
})

test_that("Error if arguments have wrong value", {
  continuous <- rtData$continuous
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords)
  # anImage <- system.file()
  
  expect_error(visualise())
  expect_error(visualise(raster = "bla"))
  expect_error(visualise(raster = continuous, geom = "bla"))
  expect_error(visualise(raster = continuous, theme = "bla"))
  expect_error(visualise(raster = continuous, trace = 1))
  expect_error(visualise(raster = continuous, image = 0))
})