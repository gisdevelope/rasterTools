library(checkmate)
library(testthat)
context("oWCLIM")


test_that("oWCLIM loads the correct file", {
  # updatePaths(root = system.file("test_datasets", package="rasterTools"))
  # myMask <- loadData(files = "aWindow.csv",
  #                    localPath = system.file("csv", package="rasterTools")) %>%
  #   geomRectangle() %>%
  #   setCRS(crs = projs$laea)
  # 
  # output <- oWCLIM(mask = myMask, )
  # expect_class(output, "RasterStack")
})

test_that(("oWCLIM works with Spatial* mask (that has another crs than the dataset)"), {
  # myMask <- loadData(files = "aWindow.csv",
  #                    localPath = system.file("csv", package="rasterTools")) %>%
  #   geomRectangle() %>%
  #   setCRS(crs = projs$laea)
  # myMask <- gToSp(geom = myMask) %>% 
  #   setCRS(crs = projs$longlat)
  # 
  # output <- oWCLIM(mask = myMask, )
  # expect_class(output, "RasterStack")
})

test_that("Error if arguments have wrong value", {
  # myMask <- loadData(files = "aWindow.csv",
  #                    localPath = system.file("csv", package="rasterTools")) %>%
  #   geomRectangle() %>%
  #   setCRS(crs = projs$laea)
  # 
  # expect_error(oWCLIM(mask = "myMask"))
  # expect_error(oWCLIM(mask = myMask, variable = 1))
  # expect_error(oWCLIM(mask = myMask, variable = "tmin", month = "bla"))
  # expect_error(oWCLIM(mask = myMask, variable = "tmin", resolution = "bla"))
  # expect_error(oWCLIM(mask = myMask, variable = "tmin", version = "bla"))
})

test_that("bibliography item has been created", {
  # myMask <- loadData(files = "aWindow.csv",
  #                    localPath = system.file("csv", package="rasterTools")) %>%
  #   geomRectangle() %>%
  #   setCRS(crs = projs$laea)
  # options(bibliography = NULL)
  # 
  # output <- oWCLIM(mask = myMask, )
  # theBib <- getOption("bibliography")
  # expect_class(theBib, classes =  "bibentry")
  # expect_list(theBib, len = 1)
})
