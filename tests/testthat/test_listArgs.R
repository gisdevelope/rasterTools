library(checkmate)
library(testthat)
context("listArgs")


test_that("listArgs returns the parent functions arguments", {
  bla <- function(x){ return(listArgs())}
  
  expect_list(bla(1), len = 1, types = "integerish")
})