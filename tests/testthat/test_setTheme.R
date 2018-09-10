library(checkmate)
library(raster)
context("setTheme")


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
  # box = list(plot  = TRUE,
  #            linewidth = 3,
  #            linetype = "solid",
  #            colour = "black")
})

test_that("modifying xAxis works", {
  continuous <- rtData$continuous
  
  # xAxis = list(plot = TRUE,
  #              bins = 4,
  #              margin = 0.05,
  #              label = list(
  #                plot = TRUE,
  #                title = "x",
  #                fontsize = 12,
  #                colour = "black",
  #                rotation = 0),
  #              ticks = list(
  #                plot = TRUE,
  #                fontsize = 10,
  #                colour = "black",
  #                digits = 1))
  myTheme <- setTheme(title = list(fontsize = 12, 
                                   colour = "grey"))
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying yAxis works", {
  continuous <- rtData$continuous
  
  # yAxis = list(plot = TRUE,
  #              bins = 4,
  #              margin = 0.05,
  #              label = list(
  #                plot = TRUE,
  #                title = "y",
  #                fontsize = 12,
  #                colour = "black",
  #                rotation = 0),
  #              ticks = list(
  #                plot = TRUE,
  #                fontsize = 10,
  #                colour = "black",
  #                digits = 1))
  myTheme <- setTheme(title = list(fontsize = 12, 
                                   colour = "grey"))
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
  
  # legend = list(plot = TRUE,
  #               common = FALSE,
  #               bins = 5,
  #               ascending = TRUE,
  #               position = "right",
  #               sizeRatio = 0.6,
  #               title = list(
  #                 plot = TRUE,
  #                 fontsize = 10,
  #                 colour = "black"),
  #               label = list(
  #                 plot = TRUE,
  #                 fontsize = 10,
  #                 colour = "black"),
  #               ticks = list(
  #                 plot = TRUE,
  #                 fontsize = 10,
  #                 colour = "black"),
  #               box = list(
  #                 plot = TRUE,
  #                 linetype = "solid",
  #                 linewidth = 1,
  #                 colour = "black"))
  myTheme <- setTheme(title = list(fontsize = 12, 
                                   colour = "grey"))
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
})

test_that("modifying geom works", {
  coords <- data.frame(x = c(40, 70, 70, 50, 40, 60, 70, 40, 60, 
                             40, 10, 20, 30, 30, 20, 50, 40, 10, 20),
                       y = c(40, 40, 60, 70, 40, 20, 40, 10, 20, 
                             40, 20, 20, 50, 40, 40, 70, 40, 20, 60),
                       id = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 
                              3, 3, 3, 4, 4, 4, 4, 4, 4, 4),
                       fid = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 
                               3, 3, 3, 4, 4, 4, 5, 5, 5, 5))
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, window = window)
  myTheme <- setTheme(geom = list(scale = list(x = "fill", to = "id"),
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

