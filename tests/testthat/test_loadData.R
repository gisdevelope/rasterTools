library(checkmate)
context("loadData")


test_that("function loads 'csv' files", {
  output <- loadData(files = "locations.csv",
                     localPath = system.file("csv", package="rasterTools"))
  expect_class(output, "geom")
})

test_that("function loads 'kml' files", {

})

test_that("function loads '' files", {

})
