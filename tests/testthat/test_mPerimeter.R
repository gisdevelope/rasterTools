library(checkmate)
context("mPerimeter")


test_that("output is data.frame", {
  input <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mPerimeter(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 26, any.missing = FALSE)

  output <- mPerimeter(obj = input, layer = "categorical", scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 48, any.missing = FALSE)

  output <- mPerimeter(obj = input, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)
})

test_that("determines patches, when binarised input is provided and 'scale = patch'", {
  input <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mPerimeter(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 26, any.missing = FALSE)

  output <- mPerimeter(obj = bin, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 2, any.missing = FALSE)
})

test_that("output has the correct columm names", {
  input <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mPerimeter(obj = bin, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "edges"))

  output <- mPerimeter(obj = input, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "edges"))

  output <- mPerimeter(obj = input, scale = "class")
  expect_names(names(output), identical.to = c("class", "edges"))
})

test_that("output with the correct unit", {
  input <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mPerimeter(obj = bin, scale = "patch", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "edgelength"))

  output <- mPerimeter(obj = input, scale = "patch", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "edgelength"))

  output <- mPerimeter(obj = input, scale = "class", unit = "map")
  expect_names(names(output), identical.to = c("class", "edgelength"))
})

test_that("Error if arguments have wrong value", {
  input <- rtData$categorical
  mat <- as.matrix(input)

  expect_error(mPerimeter(obj = mat))
  expect_error(mPerimeter(obj = input, scale = "landscape"))
  expect_error(mPerimeter(obj = input, unit = "meter"))
  expect_error(mPerimeter(obj = input, layer = 1))
})

test_that("bibliography item has been created", {
  options(bibliography = NULL)
  input <- rtData$categorical
  
  output <- mPerimeter(obj = input, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
  
  options(bibliography = NULL)
  bin <- rBinarise(rtData$continuous, thresh = 40)
  disEuc <- rDistance(bin)
  
  output <- mPerimeter(obj = input, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})
