library(checkmate)
library(testthat)
context("scaleMat")


test_that("values are scaled", {
  myMat <- matrix(nrow = 10, ncol = 10, data = runif(100))
  anotherMat <- matrix(nrow = 10, ncol = 10, data = 1)
  
  scaledVals <- scaleMat(mat = myMat, range = c(1, 10))
  expect_true(max(scaledVals) == 10)
  expect_true(min(scaledVals) == 1)
  
  newMat <- scaleMat(mat = anotherMat, range = c(5, 20))
  expect_true(all(newMat == anotherMat))
})

test_that("Error if the arguments are wrong", {
  myMat <- matrix(nrow = 10, ncol = 10, data = runif(100))
  
  expect_error(scaleMat(mat = myMat, range = 1))
})