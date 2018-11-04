## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(raster)
library(rasterTools)

coords <- data.frame(x = c(40, 70, 70, 50),
                     y = c(40, 40, 60, 70),
                     id = 1)
window <- data.frame(x = c(0, 80),
                     y = c(0, 80))
(aGeom <- geomPolygon(anchor = coords, window = window, show = TRUE))

## ------------------------------------------------------------------------
myDatasets <- list(list(operator = "oGFC", period = c(2006)),
                   list(operator = "oMODIS", product = "mod17a3", period = 2006,
                        layer = 2))

## ------------------------------------------------------------------------
library(magrittr)
myMask <- rtGeoms$mask

## ----eval=FALSE----------------------------------------------------------
#  myData <- obtain(data = myDatasets, mask = myMask)

## ------------------------------------------------------------------------
get_patches <- list(list(operator = "rBinarise", thresh = 30),
                    list(operator = "rPatches"))

## ------------------------------------------------------------------------
cc_cats <- list(get_patches = list(operator = "rBinarise", thresh = 30),
                get_patches = list(operator = "rPatches"),
                get_categories = list(operator = "rCategorise", n = 5))

## ---- fig.width=7, out.width='100%'--------------------------------------
myInput <- rtData$continuous
myPatches <- modify(input = myInput, by = get_patches, sequential = TRUE)
visualise(raster::stack(myInput, myPatches))

## ------------------------------------------------------------------------
myInput <- rtData$categorical
myMetrics <- list(a_c = list(operator = "mArea", scale = "class"),
                  a_l = list(operator = "mArea", scale = "landscape"))
measure(input = myInput, with = myMetrics)

## ------------------------------------------------------------------------
myMetric <- list(a_l = list(operator = "mArea", scale = "landscape"),
                 a_c = list(operator = "mArea", scale = "class"),
                 mCPA = "a_c / a_l * 100")

## ------------------------------------------------------------------------
myMetrics <- list(a_p = list(operator = "mArea", scale = "patch"),
                  a_c = list(operator = "mArea", scale = "class"),
                  a_l = list(operator = "mArea", scale = "landscape"),
                  mCPA = "a_c / a_l * 100",
                  mLPI = "max(a_p) / a_l * 100")

## ------------------------------------------------------------------------
measure(input = myInput, with = myMetrics)

