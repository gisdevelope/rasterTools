library(checkmate)
context("reference")


test_that("reference produces an output with at least two entries: R and rasterTools", {
  options(bibliography = NULL)

  output <- reference(print = FALSE)
  expect_list(output, len = 2, any.missing = FALSE)
  expect_class(output, "bibentry")
})

test_that("an algorithm that shall produce bib output changes the bibentry", {
  options(bibliography = NULL)
  con <- rtData$continuous
  binarised <- rBinarise(con, thresh = 30)
  patches <- rPatches(binarised)

  output <- reference(print = FALSE)
  expect_list(output, len = 3, any.missing = FALSE)
})