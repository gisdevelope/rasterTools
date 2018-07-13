#' Modify gridded objects
#'
#' Typical GIS operations modify gridded objects according to a given process.
#' This can serve to identify certain objects or to prepare the quantitative
#' assessment of the spatial object in question.
#' @param input [\code{RasterLayer(1)} | \code{list(.)} thereof]\cr an object or
#'   a named list of objects, which should be modified. Typically retrieved via
#'   \code{\link{obtain}}, but can also be assembled "by hand".
#' @param by [\code{list(.)}]\cr list of \code{operators}, in which the raster
#'   modification functions are specified. Each \code{operator} is a list
#'   iteself and includes the operator name and its arguments as sub-elements;
#'   see Examples.
#' @param sequential [\code{lgical(1)}]\cr should the defined operators be
#'   carried out based on the output of the previous operator (\code{TRUE}), or
#'   separately based on the original input (\code{FALSE}, default); see
#'   Details.
#' @param merge [\code{logical(1)}]\cr should the resulting object be merged to
#'   a raster stack (\code{TRUE}), or should it remain a list (\code{FALSE},
#'   default).
#' @param keepInput [\code{logical(1)}]\cr should \code{input} be retained
#'   (\code{TRUE}) or should it be discarded (\code{FALSE}, default)?
#' @template envir
#' @details Operators can be called several successive times with modified
#'   arguments. The following operators are recently defined...
#'
#'   ... to select a subset of cells: \itemize{ \item \code{\link{rBounded}}:
#'   Select cells with values between an upper and lower threshold in a raster.
#'   \item \code{\link{rGreater}}: Select cells with values below a threshold in
#'   a raster. \item \code{\link{rLess}}: Select cells with values above a
#'   threshold in a raster. \item \code{\link{rMask}}: Select cells of a raster
#'   based on a mask. \item \code{\link{rMatch}}: Match cells of a raster with a
#'   kernel. }
#'
#'   ... to modify cell values: \itemize{ \item \code{\link{rBinarise}}:
#'   Binarise the values in a raster. \item \code{\link{rCategorise}}: Assign
#'   categories to the values in a raster. \item \code{\link{rDistance}}:
#'   Calculate the distance map for a raster. \item \code{\link{rFillNA}}: Fill
#'   NA values in a raster. \item \code{\link{rOffset}}: Offset the values in a
#'   raster. \item \code{\link{rPermute}}: Assign a permutation to the cell
#'   values of a raster. \item \code{\link{rRange}}: Change the scale of the
#'   values in a raster. \item \code{\link{rSubstitute}}: Substitute values in a
#'   raster. }
#'
#'   ... to determine objects: \itemize{ \item \code{\link{rCentroid}}:
#'   Determine the centroid of foreground patches in a raster. \item
#'   \code{\link{rPatches}}: Determine foreground patches in a raster. \item
#'   \code{\link{rSkeletonise}}: Determine the skeleton of foreground patches in
#'   a raster. }
#'
#'   ... to morphologically modify a raster: \itemize{ \item
#'   \code{\link{rDilate}}: Morphologically dilate foreground patches in a
#'   raster. \item \code{\link{rErode}}: Morphologically erode foreground
#'   patches in a raster. }
#'
#'   ... to modify the overall raster: \itemize{ \item \code{\link{rBlend}}:
#'   Blend two rasters with each other. \item \code{\link{rReduce}}: Combine a
#'   raster stack after segregation. \item \code{\link{rRescale}}: Rescale a
#'   raster. \item \code{\link{rSegregate}}: Segregate values in a raster into
#'   layers. }
#'
#'   Moreover, you can create your own operator or check this package's
#'   \href{https://github.com/EhrmannS/rastertools}{github} page to suggest new
#'   algorithms or make a pull-request.
#' @return A list of \code{RasterLayer}s or a \code{RasterStack} of modified
#'   objects according to the number of chosen datasets and (combinations of)
#'   operators.
#' @examples
#' input <- rtData$continuous
#'
#' # employ modification with merely one operator
#' binarised <- rBinarise(input, thresh = 40)
#' visualise(binarised)
#'
#' # employ several operators combined to an algorithm, 'obj' does not need to
#' # be specified per operator in the algorithm, as 'modify' assigns it.
#' getPatches <- list(list(operator = "rBinarise", thresh = 40),
#'                    list(operator = "rPatches"))
#' patches <- modify(input, by = getPatches, sequential = TRUE)
#' visualise(patches)
#'
#' # To run separated sub-algorithms, use names for each operator to specify
#' # which elements should be computed sequentially.
#' getPatchNCats <- list(get_patches = list(operator = "rBinarise", thresh = 40),
#'                       get_patches = list(operator = "rPatches"),
#'                       get_categories = list(operator = "rCategorise", n = 5))
#' patchNCats <- modify(input, by = getPatchNCats, merge = TRUE)
#' visualise(patchNCats)
#'
#' # Create objects that are usable later in the algorithm
#' getMedialAxis <- list(skeleton = list(operator = "rSkeletonise", background = 0),
#'                       medAxis = list(operator = "rPermute"),
#'                       medAxis = list(operator = "rDistance"),
#'                       medAxis = list(operator = "rMask", mask = "skeleton"))
#' MAT <- modify(binarised, by = getMedialAxis, merge = TRUE)
#' visualise(MAT, trace = TRUE)
#' @importFrom raster stack
#' @export

modify <- function(input = NULL, by = NULL, sequential = FALSE, merge = FALSE,
                   keepInput = FALSE, envir = .GlobalEnv){

  # check arguments
  isRaster <- testClass(input, "Raster")
  isStackBrick <- testClass(input, "RasterStack")
  isList <- testClass(input, "list")
  if(!isRaster & !isStackBrick & !isList){
    stop("please provide a valid 'input'.")
  }
  if(isList){
    assertList(input, "RasterLayer")
  }
  assertList(by, types = "list", min.len = 1, any.missing = FALSE)
  assertNames(names(by[[1]]), must.include = "operator")
  assertLogical(sequential)
  assertLogical(merge)
  assertLogical(keepInput)
  assertEnvironment(envir)

  if(missing(by)){
    stop("please specify an algorithm by which to modify the spatial object.")
  }

  # check which input we are dealing with and adapt if needs be
  if(isList){
    objs <- unlist(input)
  } else if(isRaster){
    objs <- setNames(list(input), "thisObject")
  }

  # if the algos don't have names, assign generic ones and separate it into subalgos
  if(is.null(names(by))){
    if(sequential){
      names(by) <- rep("algorithm", length(by))
    } else{
      names(by) <- paste0("algorithm", seq(length(by)))
    }
  }
  subAlgoNames <- unique(names(by))
  if(keepInput){
    out <- setNames(list(input), "input")
  } else{
    out <- list()
  }

  for(j in seq_along(subAlgoNames)){

    tempObjs <- objs
    tempAlgorithm <- by[which(names(by) == subAlgoNames[j])]

    for(k in seq_along(tempAlgorithm)){

      # set the correct mask raster
      if(tempAlgorithm[[k]]$operator == "rMask"){
        if(is.character(tempAlgorithm[[k]]$mask)){
          if(tempAlgorithm[[k]]$mask == "input"){
            theMask <- input
          } else{
            theMask <- out[[which(names(out) == tempAlgorithm[[k]]$mask)]]
          }
          # warning when the string doesn't match and object of the algorithm
        }
      } else{
        theMask <- NULL
      }

      # # set the correct overlay raster
      if(tempAlgorithm[[k]]$operator == "rBlend"){
        if(is.character(tempAlgorithm[[k]]$overlay)){
          theOverlay <- theGroups <- out[[which(names(out) == tempAlgorithm[[k]]$overlay)]]
          tempAlgorithm[[k]]$overlay <- theOverlay
        }
      }

      # set the correct groups raster
      if(tempAlgorithm[[k]]$operator == "rSegregate"){
        if(is.character(tempAlgorithm[[k]]$by)){
          theGroups <- out[[which(names(out) == tempAlgorithm[[k]]$by)]]
          tempAlgorithm[[k]]$by <- theGroups
        }
      }

      # set a switch to reduce layers
      if(tempAlgorithm[[k]]$operator == "rReduce"){
        reduce <- TRUE
      } else{
        reduce <- FALSE
      }

      for(i in seq_along(tempObjs)){
        thisObj <- tempObjs[[i]]

        # if the object has more than one layer and reduce != TRUE, go
        # through each layer separately; if reduce == TRUE, treat the
        # multiple layer raster as one, because rReduce expects several
        # layers to combine them.
        if(dim(thisObj)[3] > 1 & !reduce){

          for(l in 1:dim(thisObj)[3]){

            # in case a mask has to be set and the mask contains several layers
            # (i.e. after segregating of the mask), assign the respective mask.
            if(!is.null(theMask)){
              if(dim(theMask)[3] == dim(thisObj)[3]){
                tempAlgorithm[[k]]$mask <- theMask[[l]]
              } else{
                tempAlgorithm[[k]]$mask <- theMask[[1]]
              }
            }

            modifiedObj <- do.call(what = tempAlgorithm[[k]]$operator,
                                   args = c(obj = list(thisObj[[l]]), tempAlgorithm[[k]][-1]))
            thisObj[[l]] <- modifiedObj

          }
          newHistory <- paste0("in layers: ", modifiedObj@history[[length(modifiedObj@history)]])
          thisObj@history <- c(thisObj@history, list(newHistory))
          tempObjs[[i]] <- thisObj

        } else{

          if(!is.null(theMask)){
            if(dim(theMask)[3] > 1){
              tempAlgorithm[[k]]$mask <- theMask[[1]]
              warning("only the first element of 'mask' was utilized.")
            } else{
              tempAlgorithm[[k]]$mask <- theMask
            }
          }

          modifiedObj <- do.call(what = tempAlgorithm[[k]]$operator,
                                 args = c(obj = list(thisObj), tempAlgorithm[[k]][-1]))
          tempObjs[[i]] <- modifiedObj

        }
      }

    }
    out <- c(out, setNames(object = tempObjs, nm = subAlgoNames[j]))

  }

  if(length(out) == 1){
    out <- out[[1]]
  }

  if(merge & testList(out, min.len = 2)){
    out <- stack(out)
  }

  toEnvironment(object = out, name = "new_obj_mod", envir = envir)
}

