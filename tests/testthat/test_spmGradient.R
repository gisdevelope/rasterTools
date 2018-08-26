library(checkmate)
library(raster)
context("spmGradient")


test_that("output has class RasterLayer", {
  mat <- matrix(nrow = 100, ncol = 100, data = 0)
  origin <- mat; origin[5000] <- 1
  myPointGradient <- spmGradient(mat = mat, origin = origin)
  
  output <- spmGradient(mat = mat, origin = origin)
  expect_class(output, "RasterLayer")
})

test_that("'origin' can also be a RasterLayer", {
  mat <- matrix(nrow = 100, ncol = 100, data = 0)
  obj <- raster(mat, xmn = 0, xmx = 100, ymn = 0, ymx = 100)
  obj[5000] <- 1
  
  output <- spmGradient(mat = mat, origin = obj)
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
#   output <- nlmGradient(mat = mat)
#   theBib <- getOption("bibliography")
# expect_class(theBib, classes =  "bibentry")

#
# })
#
# test_that("history is correct", {
#
# })
#
# # something about params?