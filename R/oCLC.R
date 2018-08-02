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
#' # load and outline masks from file
#' myMask <- loadData(files = "aWindow.csv",
#'                    localPath = system.file("csv", package="rasterTools")) %>%
#'   geomRectangle() %>%
#'   setCRS(crs = projs$laea)
#'
#' # extract CLC data for the derived mask
#' myCLC <- oCLC(mask = myMask, years = c(2006, 2012))
#' visualise(gridded = myCLC, trace = TRUE)
#'
#' # get the (updated) bibliography
#' reference(style = "bibtex")
#' }
#' @importFrom checkmate testClass assertIntegerish assertTRUE
#' @importFrom sp spTransform proj4string
#' @importFrom raster stack crop projectRaster colortable
#' @export

oCLC <- function(mask = NULL, years = NULL){

  # check arguments
  existsGeom <- testClass(mask, classes = "geom")
  existsSp <- testClass(mask, classes = "SpatialPolygon")
  existsSpDF <- testClass(mask, classes = "SpatialPolygonsDataFrame")
  existsSpatial <- ifelse(c(existsSp | existsSpDF), TRUE, FALSE)
  if(!existsGeom & !existsSpatial){
    stop("please provide either a SpatialPolygon* or a geom to mask with.")
  }
  assertIntegerish(years, any.missing = FALSE, min.len = 1)
  assertTRUE(all(years %in% c(1990, 2000, 2006, 2012)))

  # transform crs of the mask to the dataset crs
  target_crs <- getCRS(x = mask)
  if(target_crs != projs$laea){
    mask <- setCRS(x = mask, crs = projs$laea)
  }
  theExtent <- getExtent(x = mask)
  if(existsSpatial){
    mask <- gFrom(input = mask)
  }

  clc_out <- stack()
  history <- list()
  # go through years to extract the respective data and subset it with theExtent
  for(i in seq_along(years)){

    message(paste0("I am handling the clc datasets of the year '", years[i], "':"))
    fileName <- paste0( "g100_", substr(years[i], start = nchar(years[i])-1, stop = nchar(years[i])), ".tif")
    tempObject <- loadData(files = fileName, dataset = "clc")
    cols <- colortable(tempObject)[-1]

    history <- c(history, paste0(tempObject@history, " for the year ", years[i], ""))

    tempObject <- setCRS(x = tempObject, crs = projs$laea)

    message(paste0("  ... cropping CLC to target area"))
    tempObject <- crop(tempObject, theExtent, snap = "out")
    names(tempObject) <- paste0("landcover_", years[i])
    history <-  c(history, list(paste0("object has been cropped")))

    if(getCRS(mask) != target_crs){
      crs_name <- strsplit(target_crs, " ")[[1]][1]
      message(paste0("  ... reprojecting target CLC to '",crs_name))
      tempObject <- setCRS(x = tempObject, crs = target_crs, method = "ngb")
      history <- c(history, list(paste0("object has been reprojected to ", crs_name)))
    }

    # set colourtable
    tempObject@legend@colortable <- cols

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

  message()
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