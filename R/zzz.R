#' @useDynLib rasterTools
#' @importFrom utils packageVersion data globalVariables

.onLoad <- function(...) {
  packageDataPath <- paste0(.libPaths()[1], "/rasterTools/data/")
  pathFile <- paste0(packageDataPath, "rtPaths.rda")

  if(!file.exists(pathFile)){
    rtPaths <- list(root = NA,
                    project = NA,
                    clc = list(local = NA,
                               online = "missing"),
                    emma = list(local = NA,
                                online = "https://www.european-mammals.org/php/rendermap.php?latname=",
                                gridLinks = "http://www.helsinki.fi/~rlampine/gmap/gridfilelinks.kml"),
                    efta = list(local = NA,
                                online = "https://w3id.org/mtv/FISE/map-data"),
                    gfc = list(local = NA,
                               online = "http://commondatastorage.googleapis.com/earthenginepartners-hansen/GFC2015/"),
                    modis = list(local = NA,
                                 online = "https://e4ftl01.cr.usgs.gov/"),
                    sentinel = list(local = NA,
                                    online = "missing"),
                    worldclim = list(local = NA,
                                     online = "http://biogeo.ucdavis.edu/data/")
    )
  } else{
    load(pathFile)
  }

  options(rtPaths = rtPaths)

  invisible()
}

.onAttach <- function(...) {
  packageDataPath <- paste0(.libPaths()[1], "/rasterTools/data/")
  pathFile <- paste0(packageDataPath, "rtPaths.rda")

  assign("rtPaths", getOption("rtPaths"), envir = as.environment("package:rasterTools"))

  if(!file.exists(pathFile)){
    packageStartupMessage(paste0("Hi, I am rasterTools ", packageVersion("rasterTools"), " and I help you with your spatial data!\nI don't know where your files are stored, please see '?updatePaths' to create a suitable index."))
  } else{
    packageStartupMessage(paste0("Hi, I am rasterTools ", packageVersion("rasterTools"), " and I help you with your spatial data!"))

  }

  invisible()
}

# internal rasterTools data
globalVariables(c("rtPaths", "rtData", "theme_rt"))
# meta data
globalVariables(c("meta_modis", "meta_clc", "meta_emma", "meta_efta"))
# projections
globalVariables(c("projs"))