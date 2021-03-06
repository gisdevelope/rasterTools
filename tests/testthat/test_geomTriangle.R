library(checkmate)
context("geomTriangle")


test_that("output is valid geometry", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- geomTriangle(anchor = coords, extent = extent)
  expect_class(output, classes = "geom")
})

test_that("output has the correct number of vertices", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  output <- geomTriangle(anchor = coords, extent = extent)
  expect_data_frame(output@coords, any.missing = FALSE, nrows = 3, ncols = 4)
})

test_that("Error if arguments have wrong value", {
  coords <- data.frame(x = c(40, 70, 70, 50),
                       y = c(40, 40, 60, 70),
                       fid = 1)
  extent <- data.frame(x = c(0, 80),
                       y = c(0, 80))

  expect_error(geomTriangle(anchor = "bla"))
  expect_error(geomTriangle(anchor = coords, window = "bla"))
  expect_error(geomTriangle(anchor = coords, features = "bla"))
  expect_error(geomTriangle(template = "bla"))
})
