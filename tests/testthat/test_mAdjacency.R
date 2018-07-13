library(checkmate)
context("mAdjacency")


test_that("output is data.frame", {
  cat <- rtData$categorial
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mAdjacency(obj = cat)
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- mAdjacency(obj = bin)
  expect_data_frame(output, ncols = 2, nrows = 2, any.missing = FALSE)

  output <- mAdjacency(obj = cat, type = "paired")
  expect_data_frame(output, ncols = 10, nrows = 9, any.missing = FALSE)

  output <- mAdjacency(obj = cat, type = "pairedSum")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)

  output <- mAdjacency(obj = cat, count = "single")
  expect_data_frame(output, ncols = 2, nrows = 9, any.missing = FALSE)
})

test_that("output with the correct column names", {
  cat <- rtData$categorial
  bin <- rBinarise(rtData$continuous, thresh = 40)

  output <- mAdjacency(obj = cat)
  expect_names(names(output), identical.to = c("class", "likeAdj"))

  output <- mAdjacency(obj = cat, count = "single")
  expect_names(names(output), identical.to = c("class", "likeAdj"))

  output <- mAdjacency(obj = cat, type = "paired")
  expect_names(names(output), identical.to = c("class", "1", "11", "21", "24", "27", "31", "41", "44", "47"))

  output <- mAdjacency(obj = cat, type = "pairedSum")
  expect_names(names(output), identical.to = c("class", "pairedSum"))
})

test_that("Error if arguments have wrong value", {
  cat <- rtData$categorial
  mat <- as.matrix(cat)

  expect_error(mAdjacency(obj = mat))
  expect_error(mAdjacency(obj = cat, count = "bla"))
  expect_error(mAdjacency(obj = cat, count = 3))
  expect_error(mAdjacency(obj = cat, type = "bla"))
  expect_error(mAdjacency(obj = cat, type = 1))
})