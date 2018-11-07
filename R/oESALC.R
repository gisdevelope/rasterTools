#' Obtain global ESA CCI land-cover data
#'
#' Obtain data from the 'ESA CCI land-cover' \href{https://www.esa-landcover-cci.org/?q=node/164}{dataset}
#' (\href{https://doi.org/10.1002/joc.1276}{paper1}).
#'
#' @template mask
#' @param years [\code{integerish(.)}]\cr year(s) for which ESA CCI land-cover data should be
#'   extracted; see Details.
#' @param ... [various]\cr other arguments.
#' @family obtain operators
#' @examples
#' \dontrun{
#'
#' require(magrittr)
#'
#' myWCLIM <- oESALC(mask = rtGeoms$mask,
#'                   variable = c("tavg"),
#'                   month = c(5:9))
#' visualise(raster = myWCLIM$tavg, trace = TRUE)
#'
#' # get the (updated) bibliography
#' reference(style = "bibtex")
#' }
#' @importFrom checkmate testClass assertCharacter assertNumeric assertSubset testFileExists
#' @importFrom stringr str_split
#' @importFrom raster stack crop
#' @export

oESALC <- function(mask = NULL, years = NULL, ...){
  
  # check arguments
  maskIsGeom <- testClass(mask, classes = "geom")
  maskIsSpatial <- testClass(mask, classes = "Spatial")
  assert(maskIsGeom, maskIsSpatial)
  assertIntegerish(years, lower = 1992, upper = 2015, any.missing = FALSE, min.len = 1)

  # transform crs of the mask to the dataset crs
  if(maskIsSpatial){
    mask <- gFrom(input = mask)
  }
  targetCRS <- getCRS(x = mask)
  theExtent <- geomRectangle(anchor = getExtent(x = mask))
  theExtent <- setCRS(x = theExtent, crs = targetCRS)

  if(targetCRS != projs$longlat){
    mask <- setCRS(x = mask, crs = projs$longlat)
    targetExtent <- setCRS(theExtent, crs = projs$longlat)
  } else{
    targetExtent <- theExtent
  }
  
  out <- stack()
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
  
  #   bib <- bibentry(bibtype = "",
  #                   title = "",
  #                   author = c(person(given = "", family = "")),
  #                   journal = "",
  #                   volume = "",
  #                   year = "",
  #                   pages = "",
  #                   doi = ""
  #   )

  # if(is.null(getOption("bibliography"))){
  #   options(bibliography = bib)
  # } else{
  #   currentBib <- getOption("bibliography")
  #   if(!bib%in%currentBib){
  #     options(bibliography = c(currentBib, bib))
  #   }
  # }
  # 
  # return(wc_out)
}

#' @describeIn oESALC function to download ESA CCI land-cover data
#' @param file [\code{character(1)}]\cr the name of the file to download.
#' @template localPath
#' @importFrom httr GET write_disk progress
#' @importFrom utils unzip
#' @importFrom raster raster writeRaster
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

downloadESALC <- function(file = NULL, localPath = NULL){
  
  # assertCharacter(file, any.missing = FALSE, len = 1, null.ok = TRUE)
  # if(!is.null(localPath)){
  #   assertDirectory(localPath, access = "rw")
  # }
  # 
  # if(!is.null(file) & !is.null(localPath)){
  #   version <- ifelse(strsplit(file, "_")[[1]][1] == "wc2.0", 2, 1.4)
  #   if(version == 2){
  #     fileParts <- strsplit(file, "[.]")[[1]]
  #     middle <- strsplit(fileParts[2], "_")[[1]]
  #     file <- paste0(c(fileParts[1], paste0(middle[-length(middle)], collapse = "_"), "zip"), collapse = ".")
  #     onlinePath <- paste0(rtPaths$worldclim$remote, "worldclim/v2.0/tif/base/")
  #   } else{
  #     fileParts <- strsplit(file, "_")[[1]]
  #     end <- strsplit(fileParts[[3]], "[.]")[[1]]
  #     file <- paste0(fileParts[1], "_", paste0(end, collapse = "_"), ".zip")
  #     onlinePath <- paste0(rtPaths$worldclim$remote, "climate/worldclim/1_4/grid/cur/")
  #   }
  #   
  #   message(paste0("  ... downloading the file from '", onlinePath, "'"))
  #   GET(url = paste0(onlinePath, file),
  #       write_disk(paste0(localPath, "/", file)),
  #       progress())
  #   
  #   message(paste0("  ... unzipping the files of '", file, "'"))
  #   unzip(paste0(localPath, "/", file), exdir = localPath)
  #   
  #   # in case we deal with version 1.4, we rename the files to have sensible names
  #   if(version == 1.4){
  #     fileNames <- strsplit(file, "_")[[1]]
  #     
  #     message("  ... transforming the files to '.tif'")
  #     pb <- txtProgressBar(min = 0, max = 12, style = 3, char=">", width = getOption("width")-14)
  #     for(i in 1:12){
  #       temp <- raster(paste0(localPath, "/", paste0(fileNames[1], "_", i, ".bil")))
  #       writeRaster(temp, 
  #                   filename = paste0(localPath, "/", paste0("wc1.4_", fileNames[2], "_", fileNames[1], "_", formatC(i, width = 2, format = "d", flag = "0"), ".tif")), 
  #                   format = 'GTiff', options = "COMPRESS=DEFLATE")
  #       file.remove(c(paste0(localPath, "/", paste0(fileNames[1], "_", i, ".bil")),
  #                     paste0(localPath, "/", paste0(fileNames[1], "_", i, ".hdr"))))
  #       setTxtProgressBar(pb, i)
  #     }
  #     close(pb)
  #     
  #     file.remove(paste0(localPath, "/", file))
  #   }
  # }
}
