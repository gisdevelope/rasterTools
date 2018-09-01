library(checkmate)
library(raster)
context("setTheme")


test_that("assigning options works", {
  continuous <- rtData$continuous
  myTheme <- setTheme(from = theme_rt,
                      plot = list(title = TRUE,
                                  legend = TRUE,
                                  yAxis = TRUE,
                                  xAxis = TRUE,
                                  grid = TRUE,
                                  minorGrid = FALSE,
                                  commonScale = FALSE),
                      labels = list(yAxis = "lat", 
                                    xAxis = "lon"),
                      bins = list(yAxis = 6,
                                  xAxis = 3,
                                  legend = 6,
                                  yDigits = 1,
                                  xDigits = 2),
                      margin = list(yAxis = 0.01,
                                    xAxis = .1),
                      scale = list(raster = list(colours = c("#8A8779FF", "#FFFFFF"),
                                                 variable = "id"),
                                   geom = list(colours = c("#8A8779FF", "#FFFFFF"),
                                               variable = "id")),
                      legend = list(ascending = FALSE,
                                    position = "right",
                                    sizeRatio = .8),
                      fontsize = list(title = 18,
                                      yAxisTitle = 11,
                                      yAxisTicks = 11,
                                      xAxisTitle = 9,
                                      xAxisTicks = 9,
                                      legend = 11),
                      colour = list(title = "blue",
                                    yAxisTitle = "#000076a6",
                                    yAxisTicks = "#FFFFFF",
                                    xAxisTitle = "grey",
                                    xAxisTicks = "blue",
                                    legend = "pink",
                                    geom = "red"),
                      rotation = list(yAxisTitle = 0,
                                      xAxisTitle = 2,
                                      yAxisTicks = -45,
                                      xAxisTicks = 13),
                      fill = list(geom = "green"),
                      linetype = list(geom = "dotted"),
                      linewidth = list(geom = 1.2),
                      pointsize = list(geom = 1.2),
                      pointsymbol = list(geom = 5))
  
  output <- visualise(raster = continuous)
  expect_class(output, "recordedplot")
  
  myTheme <- setTheme(plot = list(title = FALSE))
  output <- visualise(raster = continuous, theme = myTheme)
  expect_class(output, "recordedplot")
  
})