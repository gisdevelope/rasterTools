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
#' visualise(raster = myWCLIM$tavg, trace = TRUE)
#'
#' # get the (updated) bibliography
#' reference(style = "bibtex")
#' }
#' @importFrom checkmate testClass assertCharacter assertNumeric assertSubset
#'   testFileExists
#' @importFrom stringr str_split
#' @importFrom raster stack crop
#' @export

oWCLIM <- function(mask = NULL, variable = NULL, month = c(1:12), resolution = 0.5){
  
  # check arguments
  maskIsGeom <- testClass(mask, classes = "geom")
  maskIsSpatial <- testClass(mask, classes = "Spatial")
  assert(maskIsGeom, maskIsSpatial)
  assertCharacter(variable, any.missing = FALSE, min.len = 1)
  assertSubset(variable, choices = c("tavg", "tmin", "tmax", "prec", "bio", "bio_01", "bio_02", "bio_03", "bio_04", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11", "bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19", "alt"))
  if(any(variable %in% "bio")){
    variable <- c(variable, "bio_01", "bio_02", "bio_03", "bio_04", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11", "bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18", "bio_19")
    variable <- variable[!variable %in% "bio"]
    variable <- variable[!duplicated(variable)]
  }
  assertIntegerish(month, lower = 1, upper = 12, any.missing = FALSE, min.len = 1)
  assertSubset(resolution, choices = c(0.5, 2.5, 5, 10))

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
  
  wc_out <- list()
  for(i in seq_along(variable)){
    
    thisVariable <- variable[i]
    
    # manage file names  
    if(resolution == 0.5){
      tempRes <- "30s"
    } else{
      tempRes <- paste0(resolution, "m")
    }
    if(grepl("bio", thisVariable)){
      fileNames <- paste0("wc2.0_", tempRes, "_", thisVariable, ".tif")
    } else{
      fileNames <- paste0("wc2.0_", tempRes, "_", thisVariable, "_", formatC(month, width = 2, format = "d", flag = "0"), ".tif")
    }

    history <- list()
    blablabla(paste0("I am handling the worldclim variable '", thisVariable, "':"))
    tempObject <- stack(loadData(files = fileNames, dataset = "wclim", localPath = rtPaths$worldclim$local))
    history <- c(history, paste0(tempObject[[1]]@history))
    
    blablabla(" ... cropping to targeted study area.")
    tempObject <- crop(tempObject, getExtent(x = targetExtent), snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
    history <-  c(history, list(paste0("object has been cropped")))
    if(grepl("bio", thisVariable)){
      names(tempObject) <- thisVariable
    } else{
      names(tempObject) <- paste0(thisVariable, "_", month.abb[month])
    }
    
    # reproject
    if(getCRS(mask) != targetCRS){
      crs_name <- str_split(targetCRS, " ", simplify = TRUE)[1]
      blablabla(paste0(" ... reprojecting to '", crs_name, "'."))
      tempObject <- setCRS(x = tempObject, crs = targetCRS)
      tempObject <- crop(tempObject, getExtent(x = theExtent), snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
      history <-  c(history, list(paste0("object has been reprojected to ", crs_name)))
    }
    
    tempObject@history <- history
    tempObject <- stack(tempObject)
    
    wc_out <- c(wc_out, setNames(list(tempObject), thisVariable))
  }
  
  bib <- bibentry(bibtype = "Article",
                  title = "WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas",
                  author = c(person(given = "Stephen E", family = "Fick"),
                             person(given = "Robert J", family = "Hijmans")),
                  journal = "International Journal of Climatology",
                  year = "2017",
                  doi = "10.1002/joc.5086"
  )
  blablabla()
  
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
#' @importFrom raster raster writeRaster
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

downloadWCLIM <- function(file = NULL, localPath = NULL){
  
  assertCharacter(file, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(!is.null(localPath)){
    assertDirectory(localPath, access = "rw")
  }
  
  if(!is.null(file) & !is.null(localPath)){
    fileParts <- strsplit(file, "[.]")[[1]]
    middle <- strsplit(fileParts[2], "_")[[1]]
    downFile <- paste0(c(fileParts[1], paste0(middle[-length(middle)], collapse = "_"), "zip"), collapse = ".")
    onlinePath <- paste0(rtPaths$worldclim$remote, "worldclim/v2.0/tif/base/")
    
    message(paste0("  ... downloading the file from '", onlinePath, "'"))
    GET(url = paste0(onlinePath, downFile),
        write_disk(paste0(localPath, "/", downFile)),
        progress())
    
    message(paste0("  ... unzipping the files of '", downFile, "'"))
    unzip(paste0(localPath, "/", downFile), exdir = localPath)
    
    # bio variables don't follow the same naming convention as the other files
    if(any(middle %in% "bio")){
      for(i in 1:19){
        file.rename(paste0(rtPaths$worldclim$local, "/wc2.0_", middle[3], "_", middle[2], "_", formatC(i, width = 2, format = "d", flag = "0"), ".tif"), 
                    paste0(rtPaths$worldclim$local, "/wc2.0_", middle[2], "_", middle[3], "_", formatC(i, width = 2, format = "d", flag = "0"), ".tif"))
      }
    }

  }
}
