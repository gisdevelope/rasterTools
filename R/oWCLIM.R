#' Obtain global climate data
#'
#' Obtain data from the 'worldclim 2' \href{http://worldclim.org/}{dataset}
#' (\href{https://doi.org/10.1002/joc.5086}{paper}).
#'
#' @template mask
#' @param variable [\code{character(.)}]\cr the climatic variable of interest;
#'   see Details.
#' @param month [\code{integerish(.)}]\cr the month(s) for which the data should
#'   be extracted.
#' @param resolution [\code{numeric(1)}]\cr the spatial resolution in
#'   arc-minutes.
#' @param ... [various]\cr other arguments.
#' @details The following variables are included:\itemize{ 
#'   \item \code{"tmin"}: average monthly minimum temperature [°C * 10], 
#'   \item \code{"tmax"}: average monthly maximum temperature [°C * 10], 
#'   \item \code{"tavg"}: average monthly mean temperature [°C * 10], 
#'   \item \code{"prec"}: average monthly precipitation [mm], 
#'   \item \code{"srad"}: average monthly solar radiation
#'   [kJ m-2 day-1], 
#'   \item \code{"wind"}: average monthly wind speed [m s-1],
#'   \item \code{"vapr"}: average monthly water vapour pressure [kPa],
#'   \item \code{"bio"}: all bioclimatic variables,
#'   \item \code{"bio_01"}: annual mean temperature,
#'   \item \code{"bio_02"}: mean diurnal range,
#'   \item \code{"bio_03"}: Isothermality,
#'   \item \code{"bio_04"}: temperature seasonality,
#'   \item \code{"bio_05"}: Max temperature of warmest month,
#'   \item \code{"bio_06"}: Min temperature of coldest month,
#'   \item \code{"bio_07"}: temperature annual Range,
#'   \item \code{"bio_08"}: mean temperature of wettest quarter,
#'   \item \code{"bio_09"}: mean temperature of driest quarter,
#'   \item \code{"bio_10"}: mean temperature of warmest quarter,
#'   \item \code{"bio_11"}: mean temperature of coldest quarter,
#'   \item \code{"bio_12"}: annual precipitation,
#'   \item \code{"bio_13"}: precipitation of wettest month,
#'   \item \code{"bio_14"}: precipitation of driest month,
#'   \item \code{"bio_15"}: precipitation seasonality,
#'   \item \code{"bio_16"}: precipitation of wettest quarter,
#'   \item \code{"bio_17"}: precipitation of driest quarter,
#'   \item \code{"bio_18"}: precipitation of warmest quarter,
#'   \item \code{"bio_19"}: precipitation of coldest quarter.}
#' @family obtain operators (Global)
#' @examples
#' \dontrun{
#'
#' myWCLIM <- oWCLIM(mask = rtGeoms$mask, variable = c("tavg"),
#'                   month = c(5:9))
#' visualise(raster = myWCLIM, trace = TRUE)
#'
#' # get the (updated) bibliography
#' reference(style = "bibtex")
#' }
#' @importFrom checkmate testClass assert assertCharacter assertNumeric assertSubset
#'   testFileExists
#' @importFrom raster stack raster
#' @importFrom gdalUtils gdal_translate gdalwarp
#' @export

oWCLIM <- function(mask = NULL, variable = NULL, month = c(1:12), resolution = 0.5){
  
  # check arguments
  maskIsGeom <- testClass(mask, classes = "geom")
  maskIsSp <- testClass(mask, classes = "Spatial")
  maskIsSf <- testClass(mask, classes = "sf")
  assert(maskIsGeom, maskIsSp, maskIsSf)
  if(resolution == 0.5){
    tempRes <- "30s"
  } else{
    tempRes <- paste0(resolution, "m")
  }
  assertCharacter(variable, any.missing = FALSE, min.len = 1)
  assertSubset(variable, choices = c("tavg", "tmin", "tmax", "prec", "bio", "bio_01", "bio_02", "bio_03", "bio_04", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11", "bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19", "alt"))
  if(any(variable %in% "bio")){
    variable <- c(variable, "bio_01", "bio_02", "bio_03", "bio_04", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11", "bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
    variable <- variable[!variable %in% "bio"]
  }
  if(any(!grepl("bio", variable))){
    varClim <- variable[which(!grepl("bio", variable))]
    tempVar <- NULL
    for(i in seq_along(varClim)){
      tempVar <- c(tempVar, paste0(varClim[i], "_", formatC(month, width = 2, format = "d", flag = "0")))
    }
    variable <- c(tempVar, variable[grepl("bio", variable)])
  }
  assertIntegerish(month, lower = 1, upper = 12, any.missing = FALSE, min.len = 1)
  assertSubset(resolution, choices = c(0.5, 2.5, 5, 10))

  # transform crs of the mask to the dataset crs
  targetCRS <- getCRS(x = mask)
  maskExtent <- getExtent(x = mask)
  if(targetCRS != projs$longlat){
    targetMask <- setCRS(x = mask, crs = projs$longlat)
  } else{
    targetMask <- mask
  } 
  maskGeom <- geomRectangle(anchor = getExtent(x = targetMask))
  maskGeom <- setCRS(x = maskGeom, crs = projs$longlat)
  targetExtent <- getExtent(maskGeom)
  
  out <- stack()
  for(i in seq_along(variable)){
    
    history <- list()
    message(msg = paste0("I am handling the worldclim variable '", variable[i], "' ..."))
    fileName <- paste0("wc2.0_", tempRes, "_", variable[i], ".tif")
    fileExists <- testFileExists(paste0(paste0(rtPaths$worldclim$local, "/", fileName)))
    
    if(!fileExists){
      downloadWCLIM(file = fileName,
                    localPath = rtPaths$worldclim$local)
    }
    tempObject <- gdalwarp(srcfile = paste0(rtPaths$worldclim$local, "/", fileName),
                           dstfile = paste0(rtPaths$project, "/wclim_", variable[i], "_", paste0(round(maskExtent$x), collapse = "."), "_", paste0(round(maskExtent$y), collapse = "."), ".tif"),
                           s_srs = projs$longlat,
                           t_srs = targetCRS,
                           te = c(maskExtent$x[1], maskExtent$y[1], maskExtent$x[2], maskExtent$y[2]),
                           overwrite = TRUE,
                           output_Raster = TRUE)
    
    history <- c(history, paste0("object loaded"))
    history <-  c(history, paste0("object cropped between points (x, y) '", targetExtent$x[1], ", ", targetExtent$y[1], "' and '", targetExtent$x[2], ", ", targetExtent$y[2], "'"))
    if(targetCRS != projs$longlat){
      crs_name <- strsplit(targetCRS, " ")[[1]][1]
      history <- c(history, list(paste0("object reprojected to ", crs_name)))
    }
    
    # make file available as raster
    tempObject <- raster(tempObject@file@name)
    names(tempObject) <- paste0("wclim_", variable[i])
    
    # set history
    tempObject@history <- history

    # stack all upcoming years
    out <- stack(out, tempObject)
  }
  
  bib <- bibentry(bibtype = "Article",
                  title = "WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas",
                  author = c(person(given = "Stephen E", family = "Fick"),
                             person(given = "Robert J", family = "Hijmans")),
                  journal = "International Journal of Climatology",
                  year = "2017",
                  doi = "10.1002/joc.5086"
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

#' @describeIn oWCLIM function to download global climte datasets
#' @param file [\code{character(1)}]\cr the name of the file to download.
#' @template localPath
#' @importFrom httr GET write_disk progress
#' @importFrom tools md5sum
#' @importFrom utils unzip
#' @export

downloadWCLIM <- function(file = NULL, localPath = NULL){
  
  assertCharacter(file, any.missing = FALSE, len = 1)
  if(!is.null(localPath)){
    assertDirectory(localPath, access = "rw")
  }
  
  fileParts <- strsplit(file, "[.]")[[1]]
  middle <- strsplit(fileParts[2], "_")[[1]]
  downFile <- paste0(c(fileParts[1], paste0(middle[-length(middle)], collapse = "_"), "zip"), collapse = ".")
  onlinePath <- paste0(rtPaths$worldclim$remote, "worldclim/v2.0/tif/base/")
  
  message(paste0("  ... downloading the file from '", onlinePath, "'"))
  GET(url = paste0(onlinePath, downFile),
      write_disk(paste0(localPath, "/", downFile)),
      progress())
  
  tempMD5 <- md5sum(paste0(localPath, "/", downFile))
  if(rtMD5$md5[which(rtMD5$file %in% downFile)] != tempMD5[[1]]){
    stop(paste0("the file '", downFile, "' in the directory '", localPath, "' may be damaged. See '?setMD5' for details."))
  } else{
    message(" ... MD5 checksum ok")
  }
  
  message(paste0(" ... unzipping the contents of '", downFile, "'"))
  unzip(paste0(localPath, "/", downFile), exdir = localPath)
  file.remove(c(paste0(localPath, "/", downFile), paste0(localPath, "/readme.txt")))
  
  # bio variables don't follow the same naming convention as the other files, so I rename them
  if(any(middle %in% "bio")){
    for(i in 1:19){
      file.rename(paste0(rtPaths$worldclim$local, "/wc2.0_", middle[3], "_", middle[2], "_", formatC(i, width = 2, format = "d", flag = "0"), ".tif"), 
                  paste0(rtPaths$worldclim$local, "/wc2.0_", middle[2], "_", middle[3], "_", formatC(i, width = 2, format = "d", flag = "0"), ".tif"))
    }
  }
}
