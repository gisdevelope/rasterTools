#' Obtain Corine Land Cover data
#'
#' Obtain data from the 'Corine Land Cover'
#' \href{http://land.copernicus.eu/pan-european/corine-land-cover}{dataset}
#' (currently you have to download it manually).
#'
#' @template mask
#' @param years [\code{integerish(.)}]\cr year(s) for which CLC data should be
#'   extracted; see Details.
#' @details The CLC data dataset is available for the years 1990, 2000, 2006 and
#'   2012. It covers 45 land cover classes for many European countries. It was
#'   compiled by the European Environmental Agency.
#' @return A \code{RasterLayer} of clc data.
#' @family obtain operators
#' @examples
#' \dontrun{
#'
#' require(magrittr)
#'
#' myCLC <- oCLC(mask = rtGeoms$mask, years = c(2006, 2012))
#' visualise(gridded = myCLC, trace = TRUE)
#'
#' # get the (updated) bibliography
#' reference(style = "bibtex")
#' }
#' @importFrom checkmate testClass assertIntegerish assertTRUE
#' @importFrom raster stack crop projectRaster colortable unique
#' @export

oCLC <- function(mask = NULL, years = NULL){

  # check arguments
  maskIsGeom <- testClass(mask, classes = "geom")
  maskIsSpatial <- testClass(mask, classes = "Spatial")
  assert(maskIsGeom, maskIsSpatial)
  assertIntegerish(years, any.missing = FALSE, min.len = 1)
  assertTRUE(all(years %in% c(1990, 2000, 2006, 2012)))
  
  labels <- meta_clc
  
  # transform crs of the mask to the dataset crs
  if(maskIsSpatial){
    mask <- gFrom(input = mask)
  }
  targetCRS <- getCRS(x = mask)
  theExtent <- geomRectangle(anchor = getExtent(x = mask))
  theExtent <- setCRS(x = theExtent, crs = targetCRS)
  
  if(targetCRS != projs$laea){
    mask <- setCRS(x = mask, crs = projs$laea)
    targetExtent <- setCRS(theExtent, crs = projs$laea)
  } else{
    targetExtent <- theExtent
  }

  clc_out <- stack()
  history <- list()
  # go through years to extract the respective data and subset it with theExtent
  for(i in seq_along(years)){

    message(paste0("I am handling the clc datasets of the year '", years[i], "':"))
    fileName <- paste0( "g100_", substr(years[i], start = nchar(years[i])-1, stop = nchar(years[i])), ".tif")
    tempObject <- loadData(files = fileName, dataset = "clc")
    outCols <- colortable(tempObject)[-1]

    history <- c(history, paste0(tempObject@history, " for the year ", years[i], ""))

    message(paste0("  ... cropping CLC to target area"))
    tempObject <- crop(tempObject, getExtent(x = targetExtent), snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
    names(tempObject) <- paste0("landcover_", years[i])
    history <-  c(history, list(paste0("object has been cropped")))

    if(getCRS(mask) != targetCRS){
      crs_name <- strsplit(targetCRS, " ")[[1]][1]
      message(paste0("  ... reprojecting target CLC to '",crs_name))
      tempObject <- setCRS(x = tempObject, crs = targetCRS, method = "ngb")
      history <- c(history, list(paste0("object has been reprojected to ", crs_name)))
    }
    
    # create and set RAT table
    tempObject@data@isfactor <- TRUE
    ids <- unique(tempObject)
    tempObject@data@attributes <- list(data.frame(id = ids, labels[ids,]))
    
    # set colortable
    tempObject@legend@colortable <- outCols

    # add up all upcoming years
    clc_out <- stack(clc_out, tempObject)

  }

  names(clc_out) <- paste0("clc_", years)
  clc_out@history <- history

  # manage the bibliography entry
  bib <- bibentry(bibtype = "Manual",
                  title = "{CORINE} land cover. {Technical} guide",
                  author = person("European commission"),
                  year = 1994,
                  ogranization = "OPOCE",
                  address = "Luxembourg")

  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  return(clc_out)

}