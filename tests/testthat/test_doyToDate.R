library(checkmate)
library(testthat)
context("doyToDate")


test_that("depth_list shows the right depth", {
  
  output <- doyToDate(year = 2000, doy = 055)
  expect_class(output, classes = c("POSIXct", "POSIXt"))
  
  output <- doyToDate(year = 2001, doy = c(055, 100))
  expect_class(output, classes = c("POSIXct", "POSIXt"))
})