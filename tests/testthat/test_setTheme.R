library(checkmate)
library(raster)
context("setTheme")


test_that("not plotting objects", {
  continuous <- rtData$continuous
  myTheme <- setTheme(title = list(plot = FALSE),
                      box = list(plot = FALSE),
                      xAxis = list(plot = FALSE),
                      yAxis = list(plot = FALSE),
                      grid = list(plot = FALSE),
                      legend = list(plot = FALSE))
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying title works", {
  continuous <- rtData$continuous
  
  myTheme <- setTheme(title = list(plot = FALSE))
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
  
  myTheme <- setTheme(title = list(fontsize = 12, 
                                   colour = "grey"))
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying box works", {
  continuous <- rtData$continuous
  myTheme <- setTheme(box = list(plot  = TRUE,
                                 linewidth = 5,
                                 linetype = "dashed",
                                 colour = "black"))
  
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying xAxis works", {
  continuous <- rtData$continuous
  myTheme <- setTheme(xAxis = list(plot = TRUE,
                                   bins = 8,
                                   margin = 0.01,
                                   label = list(
                                     plot = TRUE,
                                     title = "lat",
                                     fontsize = 10,
                                     colour = "black",
                                     rotation = 0),
                                   ticks = list(
                                     plot = TRUE,
                                     fontsize = 6,
                                     colour = "grey",
                                     digits = 0)))
  
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying yAxis works", {
  continuous <- rtData$continuous
  myTheme <- setTheme(yAxis = list(plot = TRUE,
                                   bins = 8,
                                   margin = 0.01,
                                   label = list(
                                     plot = TRUE,
                                     title = "lat",
                                     fontsize = 10,
                                     colour = "black",
                                     rotation = 0),
                                   ticks = list(
                                     plot = TRUE,
                                     fontsize = 6,
                                     colour = "grey",
                                     digits = 0)))
  
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying grid works", {
  continuous <- rtData$continuous
  patches <- rPatches(rBinarise(continuous, thresh = 40))
  
  myTheme <- setTheme(grid = list(plot = TRUE,
                                  minor = FALSE,
                                  colour = "black",
                                  linetype = "dashed",
                                  linewidth = 2),
                      xAxis = list(bins = 6))
  output <- visualise(raster = patches, theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying legend works", {
  continuous <- rtData$continuous
  patches <- rPatches(rBinarise(continuous, thresh = 40))
  
  myTheme <- setTheme(legend = list(plot = TRUE,
                                    common = TRUE,
                                    bins = 3,
                                    ascending = FALSE,
                                    sizeRatio = 0.8,
                                    label = list(
                                      plot = TRUE,
                                      fontsize = 8,
                                      colour = "grey"),
                                    box = list(
                                      plot = TRUE,
                                      linetype = "dashed",
                                      linewidth = 1,
                                      colour = "black")))
  output <- visualise(raster = stack(continuous, patches), theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying geom works", {
  coords <- data.frame(x = c(40, 70, 70, 50, 40, 60, 70, 40, 60, 
                             40, 10, 20, 30, 30, 20, 50, 40, 10, 20),
                       y = c(40, 40, 60, 70, 40, 20, 40, 10, 20, 
                             40, 20, 20, 50, 40, 40, 70, 40, 20, 60),
                       fid = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 
                               3, 3, 3, 4, 4, 4, 5, 5, 5, 5))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  myTheme <- setTheme(geom = list(scale = list(x = "fill", to = "fid"),
                                  line = "grey", 
                                  fill = c("#00204DFF", "#FFEA46FF"), 
                                  linetype = "dashed",
                                  linewidth = 1,
                                  pointsize = 2,
                                  pointsymbol = 3))
  output <- visualise(geom = aGeom, theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying raster works", {
  continuous <- rtData$continuous
  
  myTheme <- setTheme(raster = list(colours = terrain.colors(10)))
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
})

