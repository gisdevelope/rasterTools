library(checkmate)
library(raster)
context("rBlend")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 10
  mask <- raster::raster(m, xmn=0, xmx=60, ymn=0, ymx=56, crs=NA)

  output <- rBlend(obj = input, overlay = m)
  expect_class(output, "RasterLayer")
  
  output <- rBlend(obj = input, overlay = mask)
  expect_class(output, "RasterLayer")

  output <- rBlend(obj = input, overlay = mask, weight = 5)
  expect_class(output, "RasterLayer")

  output <- rBlend(obj = input, overlay = mask, patches = TRUE)
  expect_class(output, "RasterLayer")

  output <- rBlend(obj = input, overlay = mask, patches = TRUE, weight = 5)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 10
  mask <- raster::raster(m, xmn=0, xmx=60, ymn=0, ymx=56, crs=NA)

  output <- rBlend(obj = input, overlay = mask)
  expect_named(output)

  output <- rBlend(obj = input, overlay = mask, weight = 5)
  expect_named(output)

  output <- rBlend(obj = input, overlay = mask, patches = TRUE)
  expect_named(output)

  output <- rBlend(obj = input, overlay = mask, patches = TRUE, weight = 5)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 10
  mask <- raster::raster(m, xmn=0, xmx=60, ymn=0, ymx=56, crs=NA)
  dim1 <- dim(input)

  output <- rBlend(obj = input, overlay = mask)
  expect_equal(dim(output), dim1)

  output <- rBlend(obj = input, overlay = mask, weight = 5)
  expect_equal(dim(output), dim1)

  output <- rBlend(obj = input, overlay = mask, patches = TRUE)
  expect_equal(dim(output), dim1)

  output <- rBlend(obj = input, overlay = mask, patches = TRUE, weight = 5)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 10
  mask <- raster::raster(m, xmn=0, xmx=60, ymn=0, ymx=56, crs=NA)
  mat <- as.matrix(input)

  expect_error(rBlend(obj = mat, overlay = mask))
  expect_error(rBlend(obj = input))
  expect_error(rBlend(obj = input, overlay = "bla"))
  expect_error(rBlend(obj = input, overlay = mask, weight = "bla"))
  expect_error(rBlend(obj = input, overlay = mask, patches = "bla"))
})

test_that("weight can be fraction", {
  input <- rtData$continuous
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 10
  mask <- raster::raster(m, xmn=0, xmx=60, ymn=0, ymx=56, crs=NA)

  output <- rBlend(obj = input, overlay = mask, weight = 0.5)
  expect_class(output, "RasterLayer")
})

test_that("history is correct", {
  input <- rtData$continuous
  m <- matrix(nrow = 56, ncol = 60, data = 0)
  m[c(5:25), c(5:50)] <- 10
  mask <- raster::raster(m, xmn=0, xmx=60, ymn=0, ymx=56, crs=NA)

  output <- rBlend(obj = input, overlay = mask)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the raster has been blended")
})