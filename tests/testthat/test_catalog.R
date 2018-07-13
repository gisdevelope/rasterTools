library(checkmate)
context("catalog")


test_that("function detects files and reads their names", {
  path <- system.file("csv", package="rasterTools")
  abbr <- function(x){
    toupper(x)
  }

  output <- catalog(path, abbreviateBy = abbr, silent = TRUE)
  expect_data_frame(output, types = "character", ncols = 2, any.missing = FALSE, min.rows = 1)
})

test_that("names of output are correct", {
  path <- system.file("csv", package="rasterTools")
  abbr <- function(x){
    toupper(x)
  }

  output <- catalog(path, abbreviateBy = abbr, silent = TRUE)
  expect_names(names(output), identical.to = c("original", "abbr"))
})

