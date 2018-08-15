library(checkmate)
context("measure")


test_that("output has class data.frame", {
  input <- rtData$categorical
  myTerms <- list(a_c = list(operator = "mArea", scale = "class"),
                  a_l = list(operator = "mArea", scale = "landscape"))
  myMetrics <- list(a_p = list(operator = "mArea", scale = "patch"),
                    a_c = list(operator = "mArea", scale = "class"),
                    a_l = list(operator = "mArea", scale = "landscape"),
                    mCPA = "a_c / a_l * 100",
                    mLPI = "max(a_p) / a_l * 100")
  
  output <- measure(input = input, with = myTerms)
  expect_list(output, types = "data.frame", len = 2)
  expect_named(output, expected = c("a_c", "a_l"))
  
  output <- measure(input = input, with = myMetrics)
  expect_list(output, types = "data.frame", len = 2)
})

test_that("also lists of objects can be processed", {
  input <- list(rtData$categorical, rtData$categorical)
  myMetric <- list(a_c = list(operator = "mArea", scale = "class"),
                   a_l = list(operator = "mArea", scale = "landscape"),
                   mCPA = "a_c / a_l * 100")
  
  output <- measure(input = input, with = myMetric)
  expect_list(output, types = "list", len = 2)
  
})

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