library(checkmate)
library(testthat)
context("rtPalette")


test_that("if no steps is given, assume them", {
  efta_palette <- rtPalette(colors = c("#fcf0d400", "#e8f5c3ff", "#bfe361ff", "#7abd2aff", "#438532ff", "#16301bff", "#050707ff", "#050707ff"))  
  expect_function(efta_palette)
})

test_that("Error if the arguments are wrong", {
  expect_error(rtPalette(colors = c("#fcf0d400", "#e8f5c3ff", "#bfe361ff", "#7abd2aff", "#438532ff", "#16301bff", "#050707ff", "#050707ff"), 
                            steps = "bla"))
})