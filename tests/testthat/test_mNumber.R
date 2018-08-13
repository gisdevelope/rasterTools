library(checkmate)
context("mNumber")


test_that("output is data.frame", {
  cat <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mNumber(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 2, nrows = 1, any.missing = FALSE)

  output <- mNumber(obj = cat, scale = "patch")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- mNumber(obj = cat, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 1, any.missing = FALSE)
})

test_that("determines patches, when binarised input is provided and 'scale = patch'", {
  cat <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mNumber(obj = bin, scale = "patch")
  expect_equal(output$patches, 26)

  output <- mNumber(obj = bin, scale = "class")
  expect_equal(output$classes, 2)
})

test_that("output has the correct columm names", {
  cat <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mNumber(obj = bin, scale = "patch")
  expect_names(names(output), identical.to = c("landscape", "patches"))

  output <- mNumber(obj = cat, layer = "categorical", scale = "patch")
  expect_names(names(output), identical.to = c("class", "patches"))

  output <- mNumber(obj = cat, scale = "class")
  expect_names(names(output), identical.to = c("landscape", "classes"))
})

test_that("Error if arguments have wrong value", {
  cat <- rtData$categorical
  mat <- as.matrix(cat)

  expect_error(mNumber(obj = mat))
  expect_error(mNumber(obj = cat, scale = "landscape"))
  expect_error(mNumber(obj = cat, layer = 1))
})

test_that("bibliography item has been created", {
  cat <- rtData$categorical
  options(bibliography = NULL)
  
  output <- mNumber(obj = cat, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})