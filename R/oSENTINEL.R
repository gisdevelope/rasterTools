#' Obtain data from the Sentinel dataset
#'
#' @param mask spatial object of the area of interest of the spatial dataset.
#' @param period year of interest (YYYY), optionally concatenated to day of the
#'   year (DDD)(see Details).
#' @param band band(s) of interest. Can be numerical or a character string of
#'   the its name.
#' @param index look-up-table of abbreviations with which tile names match
#'   (built with \code{index}).
#' @param path exact path to the directory where the dataset files are located.
#'
#' @family obtain operators
#' @examples
#' \dontrun{ # first set 'options(myPath = ...)' and download files
#' ## search for the exact path, where the dataset has been stored
#' path_gfc <- listPaths(path = getOption("myPath"), pattern = c("(?i)sentinel$"),
#'                       full_names = TRUE, recursive = TRUE)
#' }

oSENTINEL <- function(mask, period, band, index, path = "."){
  # http://gisgeography.com/how-to-download-sentinel-satellite-data/
  # https://scihub.copernicus.eu/dhus/#/home
  
  skip_proc <- FALSE
  
  target_crs <- sp::proj4string(mask)
  
  # cheack reasons for which the procedure should be stoped
  
  if(!skip_proc){
    
    # figure out zone of the required tile
    # https://stackoverflow.com/questions/9186496/determining-utm-zone-to-convert-from-longitude-latitude
    ThisProj <- "+proj=UTM +zone=..."
    
    # check this again: http://www.tandfonline.com/doi/full/10.1080/2150704X.2016.1212419
    
    # control projection
    if(target_crs!=ThisProj){
      mask <- sp::spTransform(mask, ThisProj)
    }
    
    
    # https://sentinel.esa.int/web/sentinel/user-guides/sentinel-2-msi/naming-convention
    # tsentinel <- raster::raster(rgdal::readGDAL(paste0(path, )))
    cat(paste0("I am extracting the SENTINEL dataset from the local directory.\n"))
    tsentinel <- raster::raster(rgdal::readGDAL("misc/S2B_MSIL1C_20171231T103429_N0206_R108_T32UMU_20171231T123503.SAFE/GRANULE/L1C_T32UMU_A004281_20171231T103424/IMG_DATA/T32UMU_20171231T103429_B04.jp2", silent = TRUE))
    tsentinel <- raster::raster(rgdal::readGDAL("misc/S2B_MSIL1C_20171231T103429_N0206_R108_T32UMU_20171231T123503.SAFE/GRANULE/L1C_T32UMU_A004281_20171231T103424/IMG_DATA/T32UMU_20171231T103429_B08.jp2", silent = TRUE))
    
    cat(paste0("  ... cropping SENTINEL to targeted study area\n"))
    sentinel_temp <- raster::crop(tsentinel, mask, snap = "out")
    
  }
}
