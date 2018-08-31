library(checkmate)
library(testthat)
library(raster)
context("getTable")


test_that("getTable of a 'geom'", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  output <- getTable(aGeom)
  
  expect_data_frame(output, any.missing = FALSE, nrows = 1, ncols = 2)
  expect_names(names(output), identical.to = c("id", "n"))
})

test_that("getTable of a 'RasterLayer'", {
  input <- rtData$continuous
  
  # test RasterLayer without attribute table
  output <- getTable(input)
  expect_data_frame(output, any.missing = FALSE, nrows = 0, ncols = 0)

  # test RasterLayer with attribute table
  r <- raster(nrow=10, ncol=10)
  r[] <- 1; r[51:100] <- 2; r[3:6, 1:5] <- 3
  r <- ratify(r)
  rat <- raster::levels(r)[[1]]
  rat$landcover <- c('Pine', 'Oak', 'Meadow')
  rat$code <- c(12,25,30)
  levels(r) <- rat
  output <- getTable(r)
  expect_data_frame(output, any.missing = FALSE, nrows = 3, ncols = 3)
  expect_names(names(output), identical.to = c("ID", "landcover", "code"))
})
