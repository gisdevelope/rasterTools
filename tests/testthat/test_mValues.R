library(checkmate)
context("mValues")


test_that("output is data.frame", {

})

test_that("output on the correct scale", {

})

test_that("output with the correct unit", {

})

test_that("determines patches, when binarised input is provided and 'scale = patch'", {
  # bin <- rBinarise(lommData$continuous, thresh = 40)
  #
  # output <- mArea(obj = bin, scale = "patch")
  # expect_data_frame(output, ncols = 2, nrows = 27, any.missing = FALSE)
  #
  # output <- mArea(obj = bin, scale = "class")
  # expect_data_frame(output, ncols = 2, nrows = 2, any.missing = FALSE)
})

test_that("warning when input is neither binary nor with determined patches but 'scale = patch'.", {

})

test_that("Error if arguments have wrong value", {

})

test_that("bibliography item has been created", {

})