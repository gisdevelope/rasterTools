#' Measure gridded objects
#'
#' Landscape metrics typically quantify spatial patterns of 2D lattices, such as
#' maps of landscapes, artificial (spatial) patterns or even photographs.
#' @param input [\code{RasterLayer(1)} | \code{list(.)} thereof]\cr an object or
#'   a named list of objects, which should be measured. Typically retrieved via
#'   \code{\link{obtain}}, and potentially processed with \code{\link{modify}},
#'   but can also be assembled "by hand".
#' @param with [\code{character(.)}]\cr algorithm in which the operators
#'   (landscape metrics) to measure spatial patterns are specified; see
#'   Examples.
#' @param background [\code{numeric(.)}]\cr a set of values that are supposed to
#'   be regarded as background, i.e. that should not be considered for
#'   evaluating the equation(s).
#' @param simplify [\code{logical(1)}]\cr should a "nice looking" output be
#'   created, where the resulting values are associated to the correct ids
#'   (\code{TRUE}, default), or should the raw values be returned
#'   (\code{FALSE})?
#' @details A landscape metric can be generic or derived. In the first case the
#'   metric is a list that includes the \code{operator} name and its arguments.
#'   In the latter case these generic metrics are considered as \emph{terms},
#'   while the derived metric is given as its mathematical equation, where the
#'   terms are related to each other. You can find the equations for landscape
#'   metrics in the vignette with \code{vignette("landscape_metrics", package =
#'   "rasterTools")}
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
#' input <- rtData$categorical
#'
#' # calculate generic metrics 'area per class' and 'area per landscape', 'obj' does
#' # not need to be specified per operator in the algorithm, as 'measure' assigns it.
#' myMetrics <- list(a_c = list(operator = "mArea", scale = "class"),
#'                   a_l = list(operator = "mArea", scale = "landscape"))
#' (measure(input = input, with = myMetrics))
#'
#' # calculate 'class proportional area' and 'largest patch index'
#' # 1) define the terms (generic metrics) that are needed to compute the metrics
#' # 2) define the equations that are used based on these terms
#' myMetrics <- list(a_p = list(operator = "mArea", scale = "patch"),
#'                   a_c = list(operator = "mArea", scale = "class"),
#'                   a_l = list(operator = "mArea", scale = "landscape"),
#'                   mCPA = "a_c / a_l * 100",
#'                   mLPI = "max(a_p) / a_l * 100")
#' (measure(input = input, with = myMetrics, simplify = FALSE))
#'
#' # however, in the above example patches are derived per class, which might
#' # not make sense ecologically, or be at best arbitrary. Instead we might
#' # want to treat all forest, irrespective of deciduous or coniferous as one
#' # patch.
#' substituted <- rSubstitute(input, old = c(41:47), new = 40)
#' visualise(raster::stack(input, substituted))
#'
#' # now we can get perhaps more reasonable data in a simplified table
#' (measure(input = substituted, with = myMetrics))
#'
#' # To come: set of spatial operations that enables more complex metrics
#' # derive 'Disjunct Cores Density', which is a rather complex metric
#' @importFrom checkmate testClass assertList assertNamed assertLogical testList
#'   testCharacter
#' @export

measure <- function(input = NULL, with = NULL, background = NA, simplify = TRUE){

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
  assertList(with, types = c("list", "character"), min.len = 1, any.missing = FALSE)
  assertLogical(simplify)
  
  if(!isList){
    input <- list(input)
    objNames <- "thisObject"
  } else{
    objNames <- lapply(seq_along(input), function(x){
      names(input[[x]])
    })
  }
  out <- input
  
  # determine which elements are terms and which elements are equations
  isTerms <- unlist(lapply(
    seq_along(with), function(x){
      testList(with[[x]])
    }
  ))
  
  if(!any(isTerms)){
    stop("please specify at least one generic metric.")
  }
  terms <- with[isTerms]
  isEquations <- unlist(lapply(
    seq_along(with), function(x){
      testCharacter(with[[x]])
    }
  ))
  if(!any(isEquations)){
    simplify <- FALSE
  }
  equations <- with[isEquations]
  metricNames <- names(equations)

  # go through input elements
  for(i in seq_along(input)){

    value_list <- list()
    result_list <- list()
    # determine the value of each generic metric (term)
    for(j in seq_along(terms)){

      tempTerm <- terms[[j]]
      termName <- names(terms[j])
      
      # assign 'layer' in case this is missing
      if(!any(names(tempTerm) == "layer")){
        tempTerm <- c(tempTerm, list(layer = names(input[[i]])[1]))
      }
      
      # call the function and assign names
      values <- do.call(what = tempTerm$operator,
                        args = c(tempTerm[-1], obj =  input[[1]]))
      colnames(values)[!names(values) %in% c("landscape", "class", "patch", "value")] <- "result"
      values <- values[c(!values[,names(values) != "result"] %in% background),]
      
      value_list <- c(value_list, setNames(list(values), termName))
    }
    
    # make a list of IDs
    id_list <- lapply(seq_along(value_list), function(x){
      ids <- value_list[[x]]
      ids[!names(ids) %in% "result"]
    })
    
    # grab the resulting values from 'value_list'
    theValues <- lapply(seq_along(terms), function(x){
      value_list[[x]][,which(colnames(value_list[[x]]) == "result")]
    })
    names(theValues) <- names(terms)
    
    # compute the result
    for(k in seq_along(equations)){
      theResult <- round(eval(parse(text = equations[[k]]), envir = theValues), 2)
      result_list <- c(result_list, setNames(list(theResult), metricNames[k]))
    }  
    
    if(simplify){
      # get the number of rows per generic metric ...
      elemInValues <- lapply(seq_along(value_list), function(x){
        dim(value_list[[x]])[1]
      })
      
      # ... and try to match it with the results. If a generic metric has the same
      # length, the ids are what we want
      idInResults <- NULL
      tempOut <- lapply(seq_along(result_list), function(x){
        nElements <- length(result_list[[x]])
        idPos <- which(elemInValues == nElements)[[1]]
        idInResults <- c(idInResults, idPos)
        
        id_list[[idInResults]] <- data.frame(id_list[[idInResults]], result_list[[x]], fix.empty.names = FALSE)
        names(id_list[[idInResults]])[dim(id_list[[idInResults]])[2]] <- metricNames[x]
        
        id_list[[unique(idInResults)]]
      })
      out[[i]] <- tempOut
    } else{
      out[[i]] <- c(value_list, result_list)
    }
  }
  names(out) <- objNames
  
  if(length(out) == 1){
    out <- out[[1]]
  }
  
  return(out)
}
