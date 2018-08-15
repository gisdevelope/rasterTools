library(checkmate)
context("mAdjacency")


test_that("output is data.frame", {
  input <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mAdjacency(obj = input)
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- mAdjacency(obj = bin)
  expect_data_frame(output, ncols = 2, nrows = 2, any.missing = FALSE)

  output <- mAdjacency(obj = input, type = "paired")
  expect_data_frame(output, ncols = 10, nrows = 9, any.missing = FALSE)

  output <- mAdjacency(obj = input, type = "pairedSum")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- mAdjacency(obj = input, count = "single")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)
})

test_that("output with the correct column names", {
  input <- rtData$categorical
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mAdjacency(obj = input)
  expect_names(names(output), identical.to = c("class", "likeAdj"))

  output <- mAdjacency(obj = input, count = "single")
  expect_names(names(output), identical.to = c("class", "likeAdj"))

  output <- mAdjacency(obj = input, layer = "categorical", type = "paired")
  expect_names(names(output), identical.to = c("class", "1", "11", "21", "24", "27", "31", "41", "44", "47"))

  output <- mAdjacency(obj = input, type = "pairedSum")
  expect_names(names(output), identical.to = c("class", "pairedSum"))
})

test_that("Error if arguments have wrong value", {
  input <- rtData$categorical
  mat <- as.matrix(input)

  expect_error(mAdjacency(obj = mat))
  expect_error(mAdjacency(obj = input, count = "bla"))
  expect_error(mAdjacency(obj = input, count = 3))
  expect_error(mAdjacency(obj = input, type = "bla"))
  expect_error(mAdjacency(obj = input, type = 1))
})