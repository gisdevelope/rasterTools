library(checkmate)
library(testthat)
library(raster)
context("modify")


test_that("output has class RasterLayer", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  # test most simple usage
  binarise <- list(list(operator = "rBinarise", thresh = 40))
  output <- modify(input = input, by = binarise)
  expect_class(output, "RasterLayer")

  # test sequential
  closePatches <- list(list(operator = "rDilate"),
                       list(operator = "rErode"))
  output <- modify(input = binarised, by = closePatches, sequential = TRUE)
  expect_class(output, "RasterLayer")
})

test_that("'sequential = FALSE' or several sub-algos leads to a RasterStack", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  skeleton <- rSkeletonise(binarised, background = 0)

  # test sequential
  closePatches <- list(list(operator = "rDilate"),
                       list(operator = "rErode"))
  output <- modify(input = binarised, by = closePatches, sequential = FALSE, merge = TRUE)
  expect_class(output, "RasterStack")

  # test sub-algorithms (with names) and 'merge = TRUE'
  getPatchNCats <- list(get_patches = list(operator = "rBinarise", thresh = 40),
                        get_patches = list(operator = "rPatches"),
                        get_categories = list(operator = "rCategorise", n = 5))
  output <- modify(input, by = getPatchNCats, merge = TRUE)
  expect_class(output, "RasterStack")
})

test_that("input has class list", {
  myData <- list(rtData$continuous, rtData$categorical)
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  getMask <- list(list(operator = "rMask", mask = binarised))
  
  output <- modify(input = myData, by = getMask)
  expect_list(output, any.missing = FALSE, len = 2, names = "named")
})

test_that("output has class list", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  
  # test 'merge = FALSE'
  getPatchNCats <- list(get_patches = list(operator = "rBinarise", thresh = 40),
                        get_patches = list(operator = "rPatches"),
                        get_categories = list(operator = "rCategorise", n = 5))
  output <- modify(input, by = getPatchNCats, merge = FALSE)
  expect_list(output, any.missing = FALSE, len = 2, names = "named")
  
  # test 'sequential = FALSE' and 'merge = FALSE'
  closePatches <- list(list(operator = "rDilate"),
                       list(operator = "rErode"))
  output <- modify(input = binarised, by = closePatches, sequential = FALSE, merge = FALSE)
  expect_list(output, any.missing = FALSE, len = 2, names = "named")
})

test_that("rBlend is properly handled", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)
  skeleton <- rSkeletonise(binarised, background = 0)
  getMetasSkel <- list(points = list(operator = "rMatch",
                                     kernel = matrix(c(NA, 0, 0, NA, 1, 0, NA, 0, 0), 3, 3),
                                     background = 0),
                       meta = list(operator = "rBlend", overlay = "points"))
  
  output <- modify(input = skeleton, by = getMetasSkel, merge = TRUE)
  expect_class(output, "RasterStack")
  
  # # test that an overlay can be taken from the global environment (this seems to create some problem atm)
  thePoints <<- rMatch(obj = skeleton,
                   kernel = matrix(c(NA, 0, 0, NA, 1, 0, NA, 0, 0), 3, 3),
                   background = 0)
  getMetasSkel <- list(list(operator = "rBlend", overlay = "thePoints"))
  output <- modify(input = skeleton, by = getMetasSkel)
  expect_class(output, "RasterLayer")
  
  aStack <<- raster::stack(thePoints, binarised)
  getMetasSkel <- list(list(operator = "rBlend", overlay = "aStack"))
  output <- modify(input = skeleton, by = getMetasSkel)
  expect_class(output, "RasterLayer")
})

test_that("rMask is properly handled", {
  input <- rtData$continuous
  binarised <<- rBinarise(input, thresh = 30)
  aStack <<- raster::stack(binarised, input)
  
  # test that a mask can be taken from the current algorithm
  getMedialAxis <- list(skeleton = list(operator = "rSkeletonise", background = 0),
                        medAxis = list(operator = "rPermute"),
                        medAxis = list(operator = "rDistance"),
                        medAxis = list(operator = "rMask", mask = "skeleton"))
  output <- modify(input = binarised, by = getMedialAxis)
  expect_list(output, any.missing = FALSE, len = 2, names = "named")
  
  # test that also the mask "input" works
  centDistMap <- list(dis = list(operator = "rCentroid", background = 0),
                      dis = list(operator = "rBinarise", thresh = 1),
                      dis = list(operator = "rDistance"),
                      dis = list(operator = "rOffset"),
                      dis = list(operator = "rMask", mask = "input"))
  
  output <- modify(input = binarised, by = centDistMap)
  expect_class(output, "RasterLayer")
  
  # test that a mask can be taken from the global environment
  patchValues <- list(list(operator = "rMask", mask = "binarised"))
  output <- modify(input = input, by = patchValues)
  expect_class(output, "RasterLayer")
  patchValues <- list(list(operator = "rMask", mask = "aStack"))
  output <- modify(input = input, by = patchValues)
  expect_class(output, "RasterLayer")
})

test_that("rSegregate and rReduce is properly handled", {
  input <- rtData$continuous
  thePatches <<- rPatches(rBinarise(input, thresh = 30), background = 0)
  dim1 <- dim(input)
  
  getPatchValues <- list(thePatches = list(operator = "rBinarise", thresh = 30),
                         thePatches = list(operator = "rPatches", background = 0),
                         segregated = list(operator = "rSegregate", by = "thePatches"))
  output <- modify(input, by = getPatchValues)
  expect_list(output, len = 2)
  
  getPatchValues <- list(list(operator = "rSegregate", by = "thePatches"))
  output <- modify(input, by = getPatchValues)
  expect_class(output, "RasterStack")
  
  # test 'rSegregate'
  getSegPatches <- list(list(operator = "rBinarise", thresh = 30),
                        list(operator = "rPatches"),
                        list(operator = "rSegregate", flatten = TRUE, background = 0))
  output <- modify(input, by = getSegPatches, sequential = TRUE)
  expect_gt(dim(output)[3], dim1[3])
  expect_equal(dim(output)[3], 28)
  
  # test 'rReduce'
  reduceArray <- list(list(operator = "rReduce"))
  output2 <- modify(input = output, by = reduceArray)
  expect_lt(dim(output2)[3], dim(output)[3])
  expect_equal(dim(output2)[3], 1)  
  
  # test 'rSegregate' and subsequent 'rReduce'
  getPatches <- list(list(operator = "rBinarise", thresh = 30),
                     list(operator = "rPatches"),
                     list(operator = "rSegregate", flatten = TRUE, background = 0),
                     list(operator = "rReduce"))
  output <- modify(input, by = getPatches, sequential = TRUE)
  expect_equal(dim(output), dim1)
  
  # test 'rSegregate', some operation and then only 'rReduce'
  patches <- rPatches(rBinarise(input, thresh = 30))
  getCentDist <- list(patchesMask = list(operator = "rSegregate"),
                      patchesMask = list(operator = "rFillNA"),
                      distances = list(operator = "rSegregate"),
                      distances = list(operator = "rFillNA"),
                      distances = list(operator = "rCentroid", background = 0),
                      distances = list(operator = "rDistance"),
                      distances = list(operator = "rOffset"),
                      distances = list(operator = "rMask", mask = "patchesMask"),
                      distances = list(operator = "rFillNA"),
                      distances = list(operator = "rReduce", fun = max))
  centDistMap <- modify(input = patches, by = getCentDist)
  expect_list(centDistMap, len = 2)
})

test_that("input retained if 'keepInput = TRUE'", {
  input <- rtData$continuous

  # test with 'merge = TRUE'
  getPatches <- list(list(operator = "rBinarise", thresh = 40),
                     list(operator = "rPatches"))
  output <- modify(input, by = getPatches, keepInput = TRUE, sequential = TRUE)
  expect_names(names(output), identical.to = c("input", "algorithm"))

  # test with 'merge = FALSE'
  output <- modify(input, by = getPatches, keepInput = TRUE, merge = FALSE, sequential = TRUE)
  expect_names(names(output), identical.to = c("input", "algorithm"))
})

test_that("output is named", {
  input <- rtData$continuous

  # test sub-algorithms (with names) and 'merge = TRUE'
  getPatchNCats <- list(get_patches = list(operator = "rBinarise", thresh = 40),
                        get_patches = list(operator = "rPatches"),
                        get_categories = list(operator = "rCategorise", n = 5))
  output <- modify(input, by = getPatchNCats)
  expect_names(names(output), identical.to = c("get_patches", "get_categories"))

  # test with 'merge = TRUE'
  getPatches <- list(list(operator = "rBinarise", thresh = 40),
                     list(operator = "rPatches"))
  output <- modify(input, by = getPatches, keepInput = TRUE, sequential = TRUE)
  expect_names(names(output), identical.to = c("input", "algorithm"))
})

test_that("output and input have same dimension", {
  input <- rtData$continuous
  dim1 <- dim(input)

  # test 'sequential = TRUE'
  getPatches <- list(list(operator = "rBinarise", thresh = 40),
                     list(operator = "rPatches"))
  output <- modify(input, by = getPatches, sequential = TRUE)
  expect_equal(dim(output), dim1)

  # test sub-algorithms
  getPatchNCats <- list(get_patches = list(operator = "rBinarise", thresh = 40),
                        get_patches = list(operator = "rPatches"),
                        get_categories = list(operator = "rCategorise", n = 5))
  output <- modify(input, by = getPatchNCats)
  expect_equal(dim(output$get_patches), dim1)
  expect_equal(dim(output$get_categories), dim1)
})

test_that("Error if arguments have wrong value", {
  input <- rtData$continuous
  mat <- as.matrix(input)
  getPatches <- list(list(operator = "rBinarise", thresh = 40),
                     list(operator = "rPatches"))
  wrongAlgo1 <- list(list(operator = "rMask", mask = "bla"))
  wrongAlgo2 <- list(list(operator = "rMask", mask = mat))
  wrongAlgo3 <- list(list(operator = "rBlend", overlay = "bla"))
  wrongAlgo4 <- list(list(operator = "rBlend", overlay = mat))
  wrongAlgo5 <- list(list(operator = "rSegregate", by = "bla"))
  wrongAlgo6 <- list(list(operator = "rSegregate", by = mat))
  
  expect_error(modify(input = "bla", by = getPatches, sequential = TRUE))
  expect_error(modify(input = input, by = "bla", sequential = TRUE))
  expect_error(modify(input = input, by = getPatches))
  expect_error(modify(input = input, by = getPatches, sequential = "yup"))
  expect_error(modify(input = input, by = getPatches, sequential = TRUE, merge = "yup"))
  expect_error(modify(input = input, by = getPatches, sequential = TRUE, keepInput = "yup"))
  expect_error(modify(input = input, by = wrongAlgo1))
  expect_error(modify(input = input, by = wrongAlgo2))
  expect_error(modify(input = input, by = wrongAlgo3))
  expect_error(modify(input = input, by = wrongAlgo4))
})

test_that("history is correct", {
  input <- rtData$continuous
  binarised <- rBinarise(input, thresh = 30)

  # test most simple usage
  binarise <- list(list(operator = "rBinarise", thresh = 40))
  output <- modify(input, by = binarise)
  history <- output@history
  expect_list(history, types = "character", len = 2)

  # test sub-algorithms
  getPatchNCats <- list(get_patches = list(operator = "rBinarise", thresh = 40),
                        get_patches = list(operator = "rPatches"),
                        get_categories = list(operator = "rCategorise", n = 5))
  output <- modify(input, by = getPatchNCats, merge = FALSE)
  history <- output[[1]]@history
  expect_list(history, types = "character", len = 3)

  # test sub-algorithms (with names) and 'merge = TRUE'
  getPatchNCats <- list(get_patches = list(operator = "rBinarise", thresh = 40),
                        get_patches = list(operator = "rPatches"),
                        get_categories = list(operator = "rCategorise", n = 5))
  output <- modify(input, by = getPatchNCats, merge = TRUE)
  history <- output$get_patches@history
  expect_list(history, types = "character", len = 3)
})
