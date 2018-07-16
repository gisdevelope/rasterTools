library(checkmate)
library(testthat)
context("depthList")


test_that("depth_list shows the right depth", {
  algo_name <- list(list("operator1"), 
                    list("operator2"))
  
  output <- depthList(algo_name)
  expect_integerish(output, any.missing = FALSE, len = 1)
  expect_true(output == 2)
})