#' Obtain global climate data
#'
#' Obtain data from the 'worldclim' \href{http://worldclim.org/}{dataset}
#' (\href{https://doi.org/10.1002/joc.1276}{paper1},
#' \href{https://doi.org/10.1002/joc.5086}{paper2}).
#'
#' @template mask
#' @param variable [\code{character(.)}]\cr the climatic variable of interest;
#'   see Details to find which variables are available in which version.
#' @param month [\code{integerish(.)}]\cr the month(s) for which the data should
#'   be extracted.
#' @param resolution [\code{numeric(1)}]\cr the spatial resolution in
#'   arc-minutes.
#' @param version [\code{integerish(1)}]\cr the version for which you'd like to
#'   obtain the worldclim data.
#' @details \itemize{\item Version 1.4 includes the variables:\itemize{ \item
#'   \code{"tmin"}: average monthly minimum temperature [°C * 10], \item
#'   \code{"tmax"}: average monthly maximum temperature [°C * 10], \item
#'   \code{"tmean"}: average monthly mean temperature [°C * 10], \item
#'   \code{"prec"}: average monthly precipitation [mm], \item \code{"bio"}:
#'   bioclimatic variables derived from the above.} \item Version 2.0 includes
#'   the variables:\itemize{ \item \code{"tmin"}: average monthly minimum
#'   temperature [°C * 10], \item \code{"tmax"}: average monthly maximum
#'   temperature [°C * 10], \item \code{"tavg"}: average monthly mean
#'   temperature [°C * 10], \item \code{"prec"}: average monthly precipitation
#'   [mm], \item \code{"srad"}: average monthly solar radiation [kJ m-2 day-1],
#'   \item \code{"wind"}: average monthly wind speed [m s-1], \item
#'   \code{"vapr"}: average monthly water vapour pressure [kPa], \item
#'   \code{"bio"}: bioclimatic variables derived from the above.} }
#'
#'   The files of version 1.4 are shipped in the 'bil' format. While the files
#'   are compressed into a zip-file, once unzipped, they take up a lot of space
#'   on the harddisc. Hence, when using this function for the first time,
#'   \code{downloadWCLIM} downloads and unzips the original file(s), transforms
#'   the monthly files into 'tif' format (which uses only about 1/10th of the
#'   space) and deletes the original 'bil' files. Beware that due to the size of
#'   the inital files this takes a lot (!) of time, especially for the 30
#'   arc-second files. However, when repeatedly using these data, so the hope,
#'   this procedure should make the future work with these data smoother.
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
#' # extract worldclim data for the derived mask
#' myWCLIM <- oWCLIM(mask = myMask,
#'                   variable = c("tmin", "tmax"),
#'                   month = c(5:9))
#' visualise(gridded = myWCLIM$, trace = TRUE)
#'
#' # get the (updated) bibliography
#' reference(style = "bibtex")
#' }
#' @importFrom checkmate testClass assertCharacter assertNumeric assertSubset
#' @importFrom raster stack crop
#' @export

oWCLIM <- function(mask = NULL, variable = NULL, month = c(1:12), resolution = 0.5, 
                   version = 2){
  
  # check arguments
  maskIsGeom <- testClass(mask, classes = "geom")
  maskIsSpatial <- testClass(mask, classes = "Spatial")
  if(!maskIsGeom & !maskIsSpatial){
    stop("please provide either a SpatialPolygon* or a geom to mask with.")
  }
  assertCharacter(variable, any.missing = FALSE, min.len = 1)
  assertSubset(variable, choices = c("tmean", "tavg", "tmin", "tmax", "prec", "bio", "alt"))
  assertIntegerish(month, lower = 1, upper = 12, any.missing = FALSE, min.len = 1)
  assertNumeric(resolution, any.missing = FALSE, finite = TRUE, len = 1)
  assertSubset(resolution, choices = c(0.5, 2.5, 5, 10))
  assertNumeric(version, any.missing = FALSE, finite = TRUE, len = 1)
  assertSubset(version, choices = c(1.4, 2))
  
  if(version == 1.4){
    bib <- bibentry(bibtype = "Article",
                    title = "Very high resolution interpolated climate surfaces for global land areas",
                    author = c(person(given = "Robert J", family = "Hijmans"),
                               person(given = "Susan E", family = "Cameron"),
                               person(given = "Juan L", family = "Parra"),
                               person(given = "Peter G", family = "Jones"),
                               person(given = "Andy", family = "Jarvis")),
                    journal = "International Journal of Climatology",
                    volume = "25",
                    year = "2005",
                    pages = "1965-1978",
                    doi = "10.1002/joc.1276"
    )
    # make sure that the variable name is correct
    if(any(variable %in% "tavg")){
      variable[which(variable %in% "tavg")] <- "tmean"
    }
  } else{
    bib <- bibentry(bibtype = "Article",
                    title = "WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas",
                    author = c(person(given = "Stephen E", family = "Fick"),
                               person(given = "Robert J", family = "Hijmans")),
                    journal = "International Journal of Climatology",
                    year = "2017",
                    doi = "10.1002/joc.5086"
    )
    # make sure that the variable name is correct
    if(any(variable %in% "tmean")){
      variable[which(variable %in% "tmean")] <- "tavg"
    }
  }
  
  # transform crs of the mask to the dataset crs
  target_crs <- getCRS(x = mask)
  if(target_crs != projs$longlat){
    mask <- setCRS(x = mask, crs = projs$longlat)
  }
  theExtent <- getExtent(x = mask)
  
  if(maskIsSpatial){
    mask <- gFrom(input = mask)
  }
  
  # manage file names  
  if(version == 1.4){
    if(resolution == 2.5){
      resolution <- "2-5m"
    } else if(resolution == 0.5){
      resolution <- "30s"
      if(variable == "bio"){
        variable <- c("bio1-9", "bio10-19")
      }
    } else{
      resolution <- paste0(resolution, "m")
    }
    fileNames <- paste0(variable, "_", formatC(month, width = 2, format = "d", flag = "0"), "_", resolution, ".bil")
  } else{
    if(resolution == 0.5){
      resolution <- 30
      fileNames <- paste0("wc2.0_", resolution, "s_", variable, "_", formatC(month, width = 2, format = "d", flag = "0"), ".tif")
    } else{
      fileNames <- paste0("wc2.0_", resolution, "m_", variable, "_", formatC(month, width = 2, format = "d", flag = "0"), ".tif")
    }
  }
  
  history <- list()
  message(paste0("I am handling the worldclim variable '", variable, "':\n"))
  tempObject <- stack(loadData(files = fileNames, dataset = "wclim", localPath = rtPaths$worldclim$local))
  
  history <- c(history, paste0(tempObject[[1]]@history))
  # tempObject <- setCRS(x = tempObject, crs = projs$longlat)
  
  message("  ... cropping to targeted study area.\n")
  wc_out <- crop(tempObject, theExtent, snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
  history <-  c(history, list(paste0("object has been cropped")))
  names(wc_out) <- paste0(variable, "_", month.abb[month])
  
  # reproject
  if(getCRS(mask) != target_crs){
    crs_name <- strsplit(target_crs, " ")[[1]][1]
    message(paste0("  ... reprojecting to '", crs_name, "'.\n"))
    mask <- setCRS(x = mask, crs = target_crs)
    wc_out <- setCRS(x = wc_out, crs = target_crs, method = "ngb", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
    theExtent <- getExtent(x = mask)
    wc_out <- crop(wc_out, theExtent, snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
    history <-  c(history, list(paste0("object has been reprojected to ", crs_name)))
  }
  
  wc_out@history <- history
  
  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }
  
  return(wc_out)
}

#' @describeIn oWCLIM function to download global climte datasets
#' @param file [\code{character(1)}]\cr the name of the file to download.
#' @template localPath
#' @importFrom httr GET write_disk progress
#' @importFrom utils unzip
#' @importFrom raster writeRaster
#' @export

downloadWCLIM <- function(file = NULL, localPath = NULL){
  
  assertCharacter(file, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(!is.null(localPath)){
    assertDirectory(localPath, access = "rw")
  }
  
  if(!is.null(file) & !is.null(localPath)){
    version <- ifelse(strsplit(file, "_")[[1]][1] == "wc2.0", 2, 1.4)
    if(version == 2){
      fileParts <- strsplit(file, "[.]")[[1]]
      middle <- strsplit(fileParts[2], "_")[[1]]
      file <- paste0(c(fileParts[1], paste0(middle[-length(middle)], collapse = "_"), "zip"), collapse = ".")
      onlinePath <- paste0(rtPaths$worldclim$online, "worldclim/v2.0/tif/base/")
    } else{
      fileParts <- strsplit(file, "_")[[1]]
      end <- strsplit(fileParts[[3]], "[.]")[[1]]
      file <- paste0(fileParts[1], "_", paste0(end, collapse = "_"), ".zip")
      onlinePath <- paste0(rtPaths$worldclim$online, "climate/worldclim/1_4/grid/cur/")
    }
    
    message(paste0("  ... downloading the file from '", onlinePath, "'"))
    GET(url = paste0(onlinePath, file),
        write_disk(paste0(localPath, "/", file)),
        progress())
    
    message(paste0("  ... unzipping the files of '", file, "'"))
    unzip(paste0(localPath, "/", file), exdir = localPath)

    
    # in case we deal with version 1.4, we rename the files to have sensible names
    if(version == 1.4){
      fileNames <- strsplit(file, "_")[[1]]
      
      message("  ... transforming the files to '.tif'")
      pb <- txtProgressBar(min = 0, max = 12, style = 3, char=">", width = getOption("width")-14)
      for(i in 1:12){
        temp <- raster(paste0(localPath, "/", paste0(fileNames[1], "_", i, ".bil")))
        writeRaster(temp, filename = paste0(localPath, "/", paste0("wc1.4_", fileNames[2], "_", fileNames[1], "_", formatC(i, width = 2, format = "d", flag = "0"), ".tif")), 
                    format = 'GTiff', options = "COMPRESS=DEFLATE")
        file.remove(c(paste0(localPath, "/", paste0(fileNames[1], "_", i, ".bil")),
                      paste0(localPath, "/", paste0(fileNames[1], "_", i, ".hdr"))))
        setTxtProgressBar(pb, i)
      }
      close(pb)
      
      file.remove(paste0(localPath, "/", file))
    }
  }
}
