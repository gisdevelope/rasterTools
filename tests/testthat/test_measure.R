library(checkmate)
context("measure")


test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  
  wrongMetric <- list(mCPA = "a_c / a_w * 100")
  myMetric <- list(a_w = list(operator = "mArea", scale = "window"),
                   a_c = list(operator = "mArea", scale = "class"),
                   mCPA = "a_c / a_w * 100")
  
  expect_error(measure(input = "bla"))
  expect_error(measure(input = input))
  expect_error(measure(input = input, with = "bla"))
  expect_error(measure(input = input, with = wrongMetric))
  expect_error(measure(input = input, with = myMetric, simplify = "bla"))
})

test_that("history is correct", {

})