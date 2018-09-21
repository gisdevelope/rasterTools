library(checkmate)
library(testthat)
context("gToGrob")


test_that("output is valid grob", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  window <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aPolyGeom <- geomPolygon(anchor = coords, window = window, col = "blue")
  aPolyGrob <- gToGrob(geom = aPolyGeom)

  expect_list(aPolyGrob)
  expect_names(names(aPolyGrob), permutation.of = c("x", "y", "id", "id.lengths", "rule", "name", "gp", "vp"))
  expect_class(aPolyGrob, classes = c("pathgrob", "grob"))

  aPointGeom <- geomPoint(anchor = coords, window = window, col = "blue")
  aPointGrob <- gToGrob(geom = aPointGeom)
  
  expect_list(aPointGrob)
  expect_names(names(aPointGrob), permutation.of = c("x", "y", "pch", "size", "name", "gp", "vp"))
  expect_class(aPointGrob, classes = c("points", "grob"))
})

test_that("Error if arguments have wrong value", {
  notAGeom <- data.frame(x = c(25, 40, 70, 60, 30),
                         y = c(15, 25, 20, 40, 45))
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, extent = extent, col = "blue")
  
  expect_error(gToGrob(geom = notAGeom))
  expect_error(gToGrob(geom = aGeom, theme = "bla"))
})
