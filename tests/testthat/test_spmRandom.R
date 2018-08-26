library(checkmate)
context("spmRadom")


test_that("output has class RasterLayer", {
  mat <- matrix(nrow = 100, ncol = 100, data = 0)
  
  output <- spmRandom(mat = mat)
  expect_class(output, "RasterLayer")
})
#
# test_that("Error if arguments have wrong value", {
#
# })
#
# test_that("bibliography item has been created", {
#   mat <- matrix(nrow = 100, ncol = 100, data = 0)
#
#   output <- spmRandom(mat = mat)
#   theBib <- getOption("bibliography")
# expect_class(theBib, classes =  "bibentry")
#
# })
#
# test_that("history is correct", {
#
# })