## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(rasterTools)
library(raster)
library(magrittr)

## ---- fig.width=7, out.width='100%'--------------------------------------
continuous <- rtData$continuous
categorical <- rtData$categorical

visualise(raster::stack(continuous, categorical))

## ---- fig.width=7, out.width='100%'--------------------------------------
binarised <- rBinarise(categorical, match = c(41, 44, 47))

visualise(binarised)

## ---- fig.width=7, out.width='100%'--------------------------------------
opened <- rErode(binarised) %>% 
  rDilate()

visualise(opened)

## ---- fig.width=7, out.width='100%'--------------------------------------
closePatches <- list(list(operator = "rDilate"),
                     list(operator = "rErode"))

closed <- modify(input = binarised, by = closePatches, sequential = TRUE)

visualise(closed, trace = TRUE)

## ---- fig.width=7, out.width='100%'--------------------------------------
skeletonised <- binarised %>% 
  rSkeletonise() %>% 
  rFillNA()

distances <- binarised %>% 
  rPermute() %>% 
  rDistance()

MAT <- rMask(obj = distances, mask = skeletonised)
visualise(MAT)

## ---- fig.width=7, out.width='100%'--------------------------------------
getMedialAxis <- list(skeleton = list(operator = "rSkeletonise", background = 0),
                      medAxis = list(operator = "rPermute"),
                      medAxis = list(operator = "rDistance"),
                      medAxis = list(operator = "rMask", mask = "skeleton"))

MedialAxis <- modify(input = binarised, by = getMedialAxis)
visualise(MedialAxis$medAxis)

## ---- fig.width=7, out.width='100%'--------------------------------------
getPatches <- list(list(operator = "rBinarise", thresh = 30),
                   list(operator = "rPatches"))

patches <- modify(input = continuous, by = getPatches, sequential = TRUE)
visualise(patches)

## ---- fig.width=7, out.width='100%'--------------------------------------
getSegPatches <- list(list(operator = "rBinarise", thresh = 30),
                      list(operator = "rPatches"),
                      list(operator = "rSegregate", flatten = TRUE, background = 0))

foregroundPatches <- modify(input = continuous, by = getSegPatches, sequential = TRUE)
visualise(foregroundPatches[[c(2, 26)]])

## ---- fig.width=7, out.width='100%'--------------------------------------
getBGPatches <- list(background = list(operator = "rBinarise", thresh = 30),
                     background = list(operator = "rPatches"),
                     background = list(operator = "rSegregate", background = 0),
                     background = list(operator = "rBinarise", thresh = 1),
                     background = list(operator = "rPermute"),
                     background = list(operator = "rPatches"),
                     background = list(operator = "rReduce", fun = max),
                     background = list(operator = "rFillNA"))

backgroundPatches <- modify(input = continuous, by = getBGPatches)
visualise(backgroundPatches, trace = TRUE)

## ---- fig.width=7, out.width='100%'--------------------------------------
foreground <- rReduce(obj = foregroundPatches)
visualise(foreground)

## ---- fig.width=7, out.width='100%'--------------------------------------
openings <- foreground + backgroundPatches - 1
visualise(openings)

## ---- fig.width=7, out.width='100%'--------------------------------------
fullPatches <- foreground + rBinarise(openings, thresh = 1)
visualise(fullPatches)

## ---- fig.width=7, out.width='100%'--------------------------------------
findOpening <- list(patches = list(operator = "rPatches"),
                    patches = list(operator = "rFillNA"),
                    background = list(operator = "rPermute"),
                    background = list(operator = "rPatches"),
                    background = list(operator = "rFillNA"))

background <- modify(input = binarised, by = findOpening, merge = TRUE)
visualise(background)

## ---- fig.width=7, out.width='100%'--------------------------------------
skeleton <- rSkeletonise(binarised, background = 0)
kernels <- list(matrix(c(NA, 0, 0, NA, 1, 0, NA, 0, 0), 3, 3),
                matrix(c(NA, 0, 1, 1, 1, NA, NA, 0, 1), 3, 3),
                matrix(c(1, NA, 1, NA, 1, NA, NA, NA, 1), 3, 3),
                matrix(c(NA, 1, NA, 0, 1, 1, 1, 0, NA), 3, 3))

getMetaSkel <- list(endpoints = list(operator = "rMatch",
                                     kernel = matrix(c(NA, 0, 0, NA, 1, 0, NA, 0, 0), 3, 3),
                                     background = 0),
                    meta = list(operator = "rBlend", overlay = "endpoints"))

skeletonMeta <- modify(input = skeleton, by = getMetaSkel, merge = TRUE)
visualise(skeletonMeta, trace = TRUE)

## ------------------------------------------------------------------------
# devtools::install_github("AGAuffret/HistMapR")
# library(HistMapR)
# data(HistMapRex)

## ---- fig.width=7, out.width='100%'--------------------------------------
# visualise(raster = in.ras, image = TRUE)

## ---- fig.width=7, out.width='100%'--------------------------------------
centDistMap <- list(dis = list(operator = "rCentroid", background = 0),
                    dis = list(operator = "rBinarise", thresh = 1),
                    dis = list(operator = "rDistance"),
                    dis = list(operator = "rOffset"),
                    dis = list(operator = "rMask", mask = "input"),
                    dis = list(operator = "rFillNA"))

DistMap <- modify(input = binarised, by = centDistMap)
visualise(DistMap)

## ---- fig.width=7, out.width='100%'--------------------------------------
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

patchObj <- rPatches(binarised)
centDistMap <- modify(input = patchObj, by = getCentDist, keepInput = TRUE)
centDistMap <- raster::stack(centDistMap[c(1, 3)])
visualise(centDistMap)

