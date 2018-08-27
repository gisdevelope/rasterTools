#' Obtain spatial datasets
#'
#' Extract information from various spatial (gridded) datasets both from local
#' and online resources.
#' @param data [\code{list(.)}]\cr algorithm in which the operators to load
#'   spatial datasets are specified. Each \code{operator} is a list iteself and
#'   includes the operator name and its arguments as sub-elements; see Examples.
#' @param mask [\code{geom} | \code{Spatial*}]\cr spatial object. The extent of
#'   vertices that are part of the same group is used to subset the spatial
#'   information.
#' @details \code{obtain} expects a root directory in which an individual
#'   directory for each dataset dwells, where in turn all the files of this
#'   particular dataset are located. \code{rasterTools} provides a list of paths
#'   \code{getOption("rtPaths")}, which contains the directories and urls of the
#'   local and online resources. In case the dataset files are not available
#'   locally and if a url is available, an attempt to download the files is
#'   undertaken. That means typically you do not have to use the dataset
#'   specific \code{download*} functions manually, they are documented
#'   nevertheless for those who are interested. In case an error occurs, it
#'   might be necessary to specify new paths in \code{rtPaths} (see
#'   \code{\link{updatePaths}}).
#'   
#'   Recently supported datasets are \itemize{
#'   \item Global: \itemize{
#'      \item \code{\link{oGFC}}: Global Forest Change
#'      \item \code{\link{oMODIS}}: MODIS products
#'      \item \code{\link{oWCLIM}}: Worldclim
#'   }
#'   \item European: \itemize{
#'      \item \code{\link{oCLC}}: Corine Land Cover
#'      \item \code{\link{oEMMA}}: Mammal occurence in the \emph{Atlas of European Mammals}
#'      \item \code{\link{oEFTA}}: Tree presence and habitat suitability in the \emph{European Atlas of Forest Tree Species}
#'   }
#'   }
#' @return A list of objects that is organised according to the stratification
#'   in \code{data}. The first hierarchical level of the list contains all the
#'   spatial units/masks. The second level contains an entry for each dataset
#'   and its temporal extent. The third level contains the resulting output.
#'   Mostly this would be an extracted raster according to what has been
#'   specified in the arguments, but for some operators the output is a
#'   \code{data.frame} or \code{SpatialPointsDataFrame}.
#' @examples
#' require(magrittr)
#' require(rgeos)
#'
#' # specify the datasets for which you want to get data
#' myDatasets <- list(list(operator = "oGFC", years = c(2005:2007)),
#'                    list(operator = "oMODIS", product = "mod17a3", period = 2006,
#'                         layer = 2))
#'
#' # load and outline masks from a file with locations
#' myLocations <- loadData(files = "locations.csv",
#'                         localPath = system.file("csv", package="rasterTools"))
#' myMask <- gGroup(geom = myLocations, distance = 10000) %>%
#'   geomRectangle() %>%
#'   gToSp(crs = projs$laea) %>%
#'   gBuffer(width = 1000, byid = TRUE)
#'
#' \dontrun{
#' 
#' # grab the data
#' myData <- obtain(data = myDatasets, mask = myMask)
#' }
#' @importFrom stats cutree dist hclust runif setNames
#' @importFrom utils glob2rx read.csv setTxtProgressBar txtProgressBar write.csv
#' @export

obtain <- function(data = NULL, mask = NULL){

  # check arguments
  assertList(data, types = "list", min.len = 1, any.missing = FALSE)
  assertNames(names(data[[1]]), must.include = "operator")
  existsGeom <- testClass(mask, classes = "geom")
  existsSp <- testClass(mask, classes = "SpatialPolygon")
  existsSpDF <- testClass(mask, classes = "SpatialPolygonsDataFrame")
  existsSpatial <- ifelse(c(existsSp | existsSpDF), TRUE, FALSE)
  if(!existsGeom & !existsSpatial){
    stop("please provide either a SpatialPolygon* or a geom to mask with.")
  }
  
  if(existsSpatial){
    mask <- gFrom(input = mask)
  }
  theMasks <- mask
  out <- list()

  # test whether the specified operators do exists
  funs <- unlist(lapply(
    seq_along(data), function(j){
      operator <- data[[j]]$operator
      if(exists(operator)){
        operator <- operator
      } else{
        warning(paste0("operator '", operator, "' was ignored, because it does not exist."))
        operator <- FALSE
      }
      return(operator)
    }
  ))
  if(any(funs == FALSE)){
    data <- data[-which(funs == FALSE)]
    funs <- funs[-which(funs == FALSE)]
  }

  # put together a list of datasets, paths, functions and arguments
  datasets <- lapply(
    seq_along(data), function(j)
      tolower(substr(data[[j]][[1]], 2, nchar(data[[j]][[1]])))
  )

  args <- lapply(
    seq_along(data), function(j)
      c(data[[j]][-1])
  )

  # go through the defined operators and carry out a do.call for each of them
  # with the respective arguments
  tabMasks <- getTable(x = theMasks)
  maskElements <- unique(tabMasks$id)
  for(i in maskElements){
    tempMask <- getSubset(x = theMasks, subset = tabMasks$id == i)

    message(paste0("--> I am extracting information for mask ", i, ":\n"))
    temp_out <- unlist(lapply(
      seq_along(funs), function(j){
        do.call(what = funs[j],
                args = c(args[[j]], mask = list(tempMask)))
      }
    ), recursive = FALSE)

    temp_out <- setNames(temp_out, datasets)
    out <- c(out, list(temp_out))

  }
  if(length(out) == 1){
    out <- out[[1]]
  } else{
    out <- setNames(out, paste0("mask_", maskElements))
  }
  
  return(out)
}