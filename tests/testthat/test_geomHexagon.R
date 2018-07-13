library(checkmate)
context("geomHexagon")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- geomHexagon(anchor = coords, extent = extent)
  expect_class(output, classes = "geom")
})

test_that("output has the correct number of vertices", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- geomHexagon(anchor = coords, extent = extent)
  expect_data_frame(output@table, any.missing = FALSE, nrows = 6, ncols = 3)
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       id = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  expect_error(geomHexagon(anchor = "bla"))
  expect_error(geomHexagon(anchor = coords, window = "bla"))
  expect_error(geomHexagon(anchor = coords, features = "bla"))
  expect_error(geomHexagon(template = "bla"))
})
