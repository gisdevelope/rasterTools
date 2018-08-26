library(checkmate)
context("rSkeletonise")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rSkeletonise(obj = binarised)
  expect_class(output, "RasterLayer")
})

test_that("output is named", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rSkeletonise(obj = binarised)
  expect_named(output)
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)
  binarised <- rBinarise(input, thresh = 30)

  output <- rSkeletonise(obj = binarised)
  expect_equal(dim(output), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  binarised <- rBinarise(input, thresh = 30)

  expect_error(rSkeletonise(obj = mat))
  expect_error(rSkeletonise(obj = "bla"))
  expect_error(rSkeletonise(obj = binarised, kernel = c(1, 2, 3)))
  expect_error(rSkeletonise(obj = input))
})

test_that("history is correct", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  output <- rSkeletonise(obj = binarised)
  history <- output@history
  expect_list(history, len = 3)
  expect_equal(history[[3]], "the morphological skeleton has been determined")

  binarised@history <- list()
  output <- rSkeletonise(obj = binarised)
  history <- output@history
  expect_list(history, len = 2)
  expect_equal(history[[2]], "the morphological skeleton has been determined")
})

test_that("bibliography item has been created", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  
  output <- rSkeletonise(obj = binarised)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  
  mybib <- bibentry(bibtype = "Manual",
                    title = "rasterTools: obtain and process earth observation data",
                    author = person(given = "Steffen", family = "Ehrmann",
                                    email = "steffen.rasterTools@funroll-loops.de",
                                    role = c("aut", "cre")),
                    url = "https://ehrmanns.github.io/rasterTools",
                    note = paste0("version ", packageVersion("rasterTools")),
                    year = 2018)
  options(bibliography = mybib)
  output <- rSkeletonise(obj = binarised)
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})
