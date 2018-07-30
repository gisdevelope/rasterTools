library(checkmate)
context("mPerimeter")


test_that("output is data.frame", {
  cat <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mPerimeter(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 26, any.missing = FALSE)

  output <- mPerimeter(obj = cat, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 52, any.missing = FALSE)

  output <- mPerimeter(obj = cat, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)
})

test_that("determines patches, when binarised input is provided and 'scale = patch'", {
  cat <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mPerimeter(obj = bin, scale = "patch")
  expect_data_frame(output, ncols = 3, nrows = 26, any.missing = FALSE)

  output <- mPerimeter(obj = bin, scale = "class")
  expect_data_frame(output, ncols = 2, nrows = 2, any.missing = FALSE)
})

test_that("output has the correct columm names", {
  cat <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mPerimeter(obj = bin, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "edges"))

  output <- mPerimeter(obj = cat, scale = "patch")
  expect_names(names(output), identical.to = c("class", "patch", "edges"))

  output <- mPerimeter(obj = cat, scale = "class")
  expect_names(names(output), identical.to = c("class", "edges"))
})

test_that("output with the correct unit", {
  cat <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mPerimeter(obj = bin, scale = "patch", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "edgelength"))

  output <- mPerimeter(obj = cat, scale = "patch", unit = "map")
  expect_names(names(output), identical.to = c("class", "patch", "edgelength"))

  output <- mPerimeter(obj = cat, scale = "class", unit = "map")
  expect_names(names(output), identical.to = c("class", "edgelength"))
})

test_that("Error if arguments have wrong value", {
  cat <- rtData$categorical
  mat <- as.matrix(cat)

  expect_error(mPerimeter(obj = mat))
  expect_error(mPerimeter(obj = cat, scale = "landscape"))
  expect_error(mPerimeter(obj = cat, unit = "meter"))
  expect_error(mPerimeter(obj = cat, layer = 1))
})

test_that("bibliography item has been created", {
  cat <- rtData$categorical

  output <- mArea(obj = cat, scale = "class")
  theBib <- getOption("bibliography")
  expect_class(theBib, classes =  "bibentry")
})