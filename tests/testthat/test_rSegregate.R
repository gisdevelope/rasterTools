library(checkmate)
library(testthat)
library(raster)
context("rSegregate")


test_that("output has class RasterStack", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rSegregate(patches)
  expect_class(output, "RasterStack")

  output <- rSegregate(input, by = patches)
  expect_class(output, "RasterStack")
  
  output <- rSegregate(input, by = as.matrix(patches))
  expect_class(output, "RasterStack")
  
  output <- rSegregate(patches, background = 0)
  expect_class(output, "RasterStack")
})

test_that("output is named", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rSegregate(patches)
  expect_named(output)

  output <- rSegregate(input, by = patches)
  expect_named(output)

  output <- rSegregate(patches, background = 0)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)[c(1, 2)]
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rSegregate(patches)
  expect_equal(dim(output)[c(1, 2)], dim1)

  output <- rSegregate(input, by = patches)
  expect_equal(dim(output)[c(1, 2)], dim1)

  output <- rSegregate(patches, background = 0)
  expect_equal(dim(output)[c(1, 2)], dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  patches <- rPatches(rBinarise(input, thresh = 30))

  expect_error(rSegregate(mat))
  expect_error(rSegregate("bla"))
  expect_error(rSegregate(patches, by = "bla"))
  expect_error(rSegregate(patches, background = 1.1))
})

test_that("history is correct", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  output <- rSegregate(patches)
  history <- output@history
  expect_list(history, len = 4)
  expect_equal(history[[4]], "the raster has been segregated")
})

test_that("bibliography item has been created", {
  input <- rtData$continuous
  patches <- rPatches(rBinarise(input, thresh = 30))

  options(bibliography = NULL)
  output <- rSegregate(patches)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 1)
  
  mybib <- bibentry(bibtype = "Manual",
           title = "rasterTools: obtain and process earth observation data",
           author = person(given = "Steffen", family = "Ehrmann",
                           email = "steffen.rasterTools@funroll-loops.de",
                           role = c("aut", "cre")),
           url = "https://ehrmanns.github.io/rasterTools",
           note = paste0("version ", packageVersion("rasterTools")),
           year = 2018)
  options(bibliography = mybib)
  output <- rSegregate(patches)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  expect_list(theBib, len = 2)
})