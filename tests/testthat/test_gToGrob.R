library(checkmate)
library(testthat)
context("gToGrob")


test_that("output is valid grob", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))
  aGeom <- geomPolygon(anchor = coords, extent = extent, col = "blue")

  aGrob <- gToGrob(geom = aGeom)

  expect_list(aGrob)
  expect_names(names(aGrob), permutation.of = c("x", "y", "id", "id.lengths", "name", "gp", "vp"))
  expect_class(aGrob, classes = c("polygon", "grob"))
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
