library(checkmate)
library(testthat)
context("setPaths")


test_that("removing '/' at the end of a link works", {
  output <- setPaths(root = paste0(system.file("test_datasets", package="rasterTools"), "/"))
  expect_false(endsWith(output[[1]], "/"))
})

test_that("set local and remote paths", {
  output <- setPaths(root = system.file("test_datasets", package="rasterTools"),
                     local = list(sentinel = system.file("csv", package="rasterTools")),
                     remote = list(sentinel = "duckduckgo.org"))
  expect_equal(output$sentinel$local, system.file("csv", package="rasterTools"))
  expect_equal(output$sentinel$remote, "duckduckgo.org")
})

test_that("warnings if wrong specifications", {
  expect_warning(setPaths(root = system.file("test_datasets", package="rasterTools"), 
                          local = list("a/path")))
  expect_warning(setPaths(root = system.file("test_datasets", package="rasterTools"), 
                          remote = list("a/url")))
})

test_that("Error if arguments have wrong value", {
  expect_error(setPaths())
})