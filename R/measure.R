#' Measure gridded objects
#'
#' Landscape metrics typically quantify spatial patterns of 2D lattices, such as
#' maps of landscapes, artificial (spatial) patterns or even photographs. See
#' \code{vignette("landscape_metrics", package = "rasterTools")} for details on how
#' landscape metrics are treated in \code{rasterTools}.
#' @param input [\code{RasterLayer(1)} | \code{list(.)} thereof]\cr an object or
#'   a named list of objects, which should be measured. Typically retrieved via
#'   \code{\link{obtain}}, and potentially processed with \code{\link{modify}},
#'   but can also be assembled "by hand".
#' @param with [\code{character(.)}]\cr The landscape metric names.
#' @param simplify [\code{logical(1)}]\cr should only the scale of the first
#'   term and the result be returned (\code{TRUE}, default), or should the the
#'   result of all terms be returned \code{FALSE}?
#' @details A landscape metric can be generic or derived. In the first case the
#'   metric is a list that includes the \code{operator} name and its arguments.
#'   In the latter case the metric is a named list of generic metrics (socalled
#'   terms) and an equation, which determines the term's relationship.
#'
#'   The following operators for generic metrics are defined: \itemize{ \item
#'   \code{\link{mAdjacency}}: Determine the adjacency matrix of a raster. \item
#'   \code{\link{mArea}}: Calculate the area of objects in a raster. \item
#'   \code{\link{mNumber}}: Count the number of objects in a raster. \item
#'   \code{\link{mPerimeter}}: Calculate the length of the boundary of objects
#'   in a raster. \item \code{\link{mValues}}: Summarise the values of objects
#'   in a raster. }
#' @return depending on the employed metric, but typically a \code{data.frame}.
#' @examples
#' input <- rtData$categorial
#'
#' # calculate generic metrics 'area per class' and 'area per window', 'obj' does
#' # not need to be specified per operator in the algorithm, as 'measure' assigns it.
#' mAc <- list(operator = "mArea", scale = "class")
#' mAw <- list(operator = "mArea", scale = "window")
#' (measure(input = input, with = c("mAc", "mAw")))
#'
#' # calculate derived metrics 'largest patch index' and
#' # 'class proportional area'
#' visualise(input)
#' (measure(input = input, with = c("mLPI", "mCPA"), simplify = FALSE))
#'
#' # when a categorial raster is the basis to derive patches, there may be a
#' # number of patches that may in fact not be distinct patches, so one has to
#' # be carefull in interpreting these results. Instead, consider:
#' substituted <- rSubstitute(input, old = c(41:47), new = 40)
#' visualise(substituted)
#' (measure(input = substituted, with = c("mLPI", "mCPA")))
#'
#'
#' # set of spatial operations that enables more complex metrics
#' # derive 'Disjunct Cores Density', which is a rather complex metric
#' @export

measure <- function(input, with, simplify = TRUE){

  # check arguments
  isRaster <- testClass(input, "Raster")
  isRasterStack <- testClass(input, "RasterStack")
  isList <- testClass(input, "list")
  if(!isRaster & !isRasterStack & !isList){
    stop("please provide an 'input'.")
  }
  if(isList){
    assertList(input, "RasterLayer")
  }
  assertCharacter(with, any.missing = FALSE, min.len = 1)

  if(!isList){
    input <- list(input)
  }
  out <- input

  for(i in seq_along(input)){

    value_list <- list()
    for(j in seq_along(with)){

      tempMetric <- get(with[j])

      # make sure that tempMetric has the correct depth
      if(depthList(tempMetric) == 1){
        tempMetric <- list(tempMetric)
      }

      # in case the metric contains an equation, solve the equation
      if(any(names(tempMetric) == "equation")){

        # find equation and terms
        equation <- tempMetric[which(names(tempMetric) == "equation")][[1]]
        terms <- tempMetric[which(names(tempMetric) != "equation")]
        # complement 'terms' with the respective layer, if not assigned
        # otherwise
        for(k in seq_along(terms)){
          if(!any(names(terms[[k]]) == "layer")){
            terms[[k]] <- c(terms[[k]], list(layer = names(input[[i]])[1]))
          }
        }

        # derive the result per term
        valuesAll <- lapply(seq_along(terms), function(x){
          temp <- do.call(what = terms[[x]]$operator,
                          args = c(terms[[x]][-1], obj = input[[1]]))
          colnames(temp)[!names(temp) %in% c("window", "class", "patch")] <- names(terms)[x]
          return(temp)
        })
        theValues <- lapply(seq_along(terms), function(x){
          valuesAll[[x]][,which(colnames(valuesAll[[x]]) == names(terms)[x])]
        })
        names(theValues) <- names(terms)

        # merge all lists
        values <- Reduce(merge, valuesAll)

        # solve the equation
        theResult <- round(eval(parse(text = equation), envir = theValues), 2)
        values <- cbind(values, result = theResult)
        metricName <- with[j]

        if(simplify){
          values <- cbind(valuesAll[[1]][1], theResult)
          names(values)[2] <- metricName
        }

        # } else if(any(names(tempMetric) == "which")){ to identify certain patches after calculating their statistics


      } else{ # no equation, hence a generic metric

        tempMetric <- unlist(tempMetric, recursive = FALSE)
        # assign 'layer' in case this is missing
        if(!any(names(tempMetric) == "layer")){
          tempMetric <- c(tempMetric, list(layer = names(input[[i]])[1]))
        }

        # call the function and assign names
        values <- do.call(what = tempMetric$operator,
                          args = c(tempMetric[-1], obj =  input[[1]]))
        colnames(values)[!names(values) %in% c("window", "class", "patch")] <- "result"
        metricName <- with[j]

      }
      # put together the output list
      valsList <- setNames(list(values), metricName)
      value_list <- c(value_list, valsList)

    }
    out[[i]] <- value_list

  }

  # if length is 1, it was most likely not a list but is in any cased not saved as list.
  if(length(out) == 1){
    out <- out[[1]]
  }
 
  return(out)
}
