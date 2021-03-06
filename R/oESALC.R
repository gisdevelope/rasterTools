#' Obtain global ESA CCI land-cover data
#'
#' Obtain data from the 'ESA CCI land-cover'
#' \href{http://maps.elie.ucl.ac.be/CCI/viewer/index.php}{dataset}
#' (\href{http://maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf}{User
#' Guide}).
#'
#' @template mask
#' @param years [\code{integerish(.)}]\cr year(s) for which ESA CCI land-cover
#'   data should be extracted; see Details.
#' @param assertQuality [\code{logical(1)}] (not supported yet)\cr should the
#'   quality flags of the ESA CCI land-cover dataset be extracted (\code{TRUE},
#'   default) or should merely the data-layer be extracted (\code{FALSE})?
#' @param ... [various]\cr other arguments.
#' @family obtain operators (Global)
#' @examples
#' \dontrun{
#'
#' myESALC <- oESALC(mask = rtGeoms$mask, years = 2005)
#' visualise(raster = myESALC, trace = TRUE)
#'
#' # get the (updated) bibliography
#' reference(style = "bibtex")
#' }
#' @importFrom checkmate testClass assert assertIntegerish testFileExists
#' @importFrom raster stack raster unique
#' @importFrom gdalUtils gdal_translate gdalwarp
#' @export

oESALC <- function(mask = NULL, years = NULL, assertQuality = TRUE, ...){
  
  # check arguments
  maskIsGeom <- testClass(mask, classes = "geom")
  maskIsSp <- testClass(mask, classes = "Spatial")
  maskIsSf <- testClass(mask, classes = "sf")
  assert(maskIsGeom, maskIsSp, maskIsSf)
  assertIntegerish(years, lower = 1992, upper = 2015, any.missing = FALSE, min.len = 1)

  # transform crs of the mask to the dataset crs
  targetCRS <- getCRS(x = mask)
  maskGeom <- geomRectangle(anchor = getExtent(x = mask))
  maskGeom <- setCRS(x = maskGeom, crs = targetCRS)
  maskExtent <- getExtent(maskGeom)
  if(targetCRS != projs$longlat){
    targetExtent <- getExtent(setCRS(x = maskGeom, crs = projs$longlat))
  } else{
    targetExtent <- getExtent(maskGeom)
  } 
  
  out <- stack()
  # go through years to extract the respective data and subset it
  for(i in seq_along(years)){
    
    history <- list()
    message(msg = paste0("I am handling the ESALC datasets of the year '", years[i], "':"))
    fileName <- paste0( "ESACCI-LC-L4-LCCS-Map-300m-P1Y-", years[i], "-v2.0.7.tif")
    fileExists <- testFileExists(paste0(paste0(rtPaths$esalc$local, "/", fileName)))
    
    if(!fileExists){
      downloadESALC(file = fileName,
                    localPath = rtPaths$esalc$local)
    }
    message(paste0(" ... cropping ESALC to 'mask'"))
    tempObject <- gdal_translate(src_dataset = paste0(rtPaths$esalc$local, "/", fileName),
                                 dst_dataset = paste0(rtPaths$project, "/esalc_", years[i], "_", paste0(round(maskExtent$x), collapse = "."), "_", paste0(round(maskExtent$y), collapse = "."), ".tif"),
                                 projwin = c(targetExtent$x[1], targetExtent$y[2], targetExtent$x[2], targetExtent$y[1]),
                                 output_Raster = TRUE)
    
    history <- c(history, paste0("object loaded for the year ", years[i], ""))
    history <-  c(history, paste0("object cropped between points (x, y) '", targetExtent$x[1], ", ", targetExtent$y[1], "' and '", targetExtent$x[2], ", ", targetExtent$y[2], "'"))
    
    # reproject
    if(targetCRS != projs$longlat){
      crs_name <- strsplit(targetCRS, " ")[[1]][1]
      message(paste0(" ... reprojecting target to '", crs_name))
      tempObject <- gdalwarp(srcfile = paste0(rtPaths$project, "/esalc_", years[i], "_", paste0(round(maskExtent$x), collapse = "."), "_", paste0(round(maskExtent$y), collapse = "."), ".tif"),
                             dstfile = paste0(rtPaths$project, "/esalc_", years[i], "_", paste0(round(maskExtent$x), collapse = "."), "_", paste0(round(maskExtent$y), collapse = "."), "_warped.tif"),
                             s_srs = projs$longlat,
                             t_srs = targetCRS,
                             te = c(maskExtent$x[1], maskExtent$y[1], maskExtent$x[2], maskExtent$y[2]),
                             overwrite = TRUE,
                             output_Raster = TRUE)
      history <- c(history, list(paste0("object reprojected to ", crs_name)))
    }
    
    # make file available as raster
    tempObject <- raster(tempObject@file@name)
    names(tempObject) <- paste0("esalc_", years[i])
    
    # set history
    tempObject@history <- history
    
    # create and set RAT
    tempObject@data@isfactor <- TRUE
    ids <- unique(tempObject)
    ids <- ids[!is.na(ids)]
    tempObject@data@attributes <- list(data.frame(id = ids, label = meta_esalc$LCCOwnLabel[which(meta_esalc$NB_LAB %in% ids)]))
    
    # set colortable
    outCols <- rep("#000000", 256)
    outCols[meta_esalc$NB_LAB] <- meta_esalc$colour[-1]
    tempObject@legend@colortable <- outCols
    
    # stack all upcoming years
    out <- stack(out, tempObject)
  }
  
  out@history <- list("please see each RasterLayer's individual history")
  
  bib <- bibentry(bibtype = "Manual",
                  title = "Product User Guide",
                  author = person("Land Cover CCI"),
                  version = 2.0,
                  year = "2017",
                  pages = "105"
  )

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

#' @describeIn oESALC function to download ESA CCI land-cover data
#' @param file [\code{character(1)}]\cr the name of the file to download.
#' @template localPath
#' @importFrom checkmate assertCharacter assertDirectory
#' @importFrom httr GET write_disk progress
#' @importFrom tools md5sum
#' @export

downloadESALC <- function(file = NULL, localPath = NULL){
  
  assertCharacter(file, any.missing = FALSE, len = 1)
  if(!is.null(localPath)){
    assertDirectory(localPath, access = "rw")
  }

  onlinePath <- rtPaths$esalc$remote
  blablabla(paste0(" ... downloading the file from '", onlinePath, "'"))
  suppressWarnings(
    GET(url = paste0(onlinePath, file),
        authenticate(user = "anonymous", password = "rasterTools@funroll-loops.de"),
        write_disk(paste0(localPath, "/", file)),
        progress())
  )
  
  tempMD5 <- md5sum(paste0(localPath, "/", file))
  if(rtMD5$md5[which(rtMD5$file %in% file)] != tempMD5[[1]]){
    stop(paste0("the file '", file, "' in the directory '", localPath, "' may be damaged. See '?setMD5' for details."))
  } else{
    blablabla(" ... MD5 checksum ok")
  }

}
