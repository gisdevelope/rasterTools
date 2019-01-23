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
#' @family obtain operators (Europe)
#' @examples
#' \dontrun{
#'
#' myCLC <- oCLC(mask = rtGeoms$mask, years = c(2006, 2012))
#' visualise(raster = myCLC, trace = TRUE)
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
  maskIsSp <- testClass(mask, classes = "Spatial")
  maskIsSf <- testClass(mask, classes = "sf")
  assert(maskIsGeom, maskIsSp, maskIsSf)
  assertIntegerish(years, any.missing = FALSE, min.len = 1)
  assertTRUE(all(years %in% c(1990, 2000, 2006, 2012)))

  labels <- meta_clc
  
  # transform crs of the mask to the dataset crs
  targetCRS <- getCRS(x = mask)
  maskExtent <- getExtent(x = mask)
  if(targetCRS != projs$sinu){
    targetMask <- setCRS(x = mask, crs = projs$sinu)
  } else{
    targetMask <- mask
  } 
  maskGeom <- geomRectangle(anchor = getExtent(x = targetMask))
  maskGeom <- setCRS(x = maskGeom, crs = projs$sinu)
  targetExtent <- getExtent(maskGeom)
  

  out <- stack()
  history <- list()
  # go through years to extract the respective data and subset it with theExtent
  for(i in seq_along(years)){

    blablabla(paste0("I am handling the clc datasets of the year '", years[i], "' ..."))
    fileName <- paste0( "g100_", substr(years[i], start = nchar(years[i])-1, stop = nchar(years[i])), ".tif")
    fileExists <- testFileExists(paste0(rtPaths$clc$local, "/", fileName))
    
    if(!fileExists){
      stop(paste0("please download the CLC_", years[i], " datasets and store the raster in '", rtPaths$clc$local, "' with the name '", fileName, "'."))
    }
    shortName <- strsplit(fileName, "[.]")[[1]]
    tempObject <- gdalwarp(srcfile = paste0(rtPaths$clc$local, "/", fileName),
                           dstfile = paste0(rtPaths$project, "/clc_", shortName[1], "_", paste0(round(maskExtent$x), collapse = "."), "_", paste0(round(maskExtent$y), collapse = "."), ".tif"),
                           s_srs = projs$laea,
                           t_srs = targetCRS,
                           te = c(maskExtent$x[1], maskExtent$y[1], maskExtent$x[2], maskExtent$y[2]),
                           overwrite = TRUE,
                           output_Raster = TRUE)[[1]]
    
    outCols <- colortable(tempObject)[-1]
    
    history <- c(history, paste0("object loaded for the year ", years[i], ""))
    history <-  c(history, list(paste0("object has been cropped")))
    if(targetCRS != projs$laea){
      crs_name <- strsplit(targetCRS, " ")[[1]][1]
      history <- c(history, list(paste0("object has been reprojected to ", crs_name)))
    }
    
    # make file available as raster
    tempObject <- raster(tempObject@file@name)
    names(tempObject) <- paste0("clc_", years[i])

    # set history
    tempObject@history <- history
    
    # create and set RAT
    tempObject@data@isfactor <- TRUE
    ids <- unique(tempObject)
    ids <- ids[!is.na(ids)]
    tempObject@data@attributes <- list(data.frame(id = ids, labels[ids,c(1:4)]))

    # set colortable
    outCols <- rep("#000000", 256)
    outCols[meta_clc$value] <- meta_clc$colour
    tempObject@legend@colortable <- outCols
    
    # add up all upcoming years
    out <- stack(out, tempObject)
  }

  names(out) <- paste0("clc_", years)
  out@history <- history

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

  return(out)

}
