#' @useDynLib rasterTools
#' @importFrom utils packageVersion data globalVariables

.onLoad <- function(...) {
  packageDataPath <- paste0(.libPaths()[1], "/rasterTools/data/")
  pathFile <- paste0(packageDataPath, "rtPaths.rda")

  if(!file.exists(pathFile)){
    rtPaths <- list(root = NA,
                    clc = list(local = NA,
                               remote = "missing"),
                    emma = list(local = NA,
                                remote = "https://www.european-mammals.org/php/rendermap.php?latname=",
                                gridLinks = "http://www.helsinki.fi/~rlampine/gmap/gridfilelinks.kml"),
                    efta = list(local = NA,
                                remote = "https://w3id.org/mtv/FISE/map-data"),
                    gfc = list(local = NA,
                               remote = "http://commondatastorage.googleapis.com/earthenginepartners-hansen/GFC2015/"),
                    modis = list(local = NA,
                                 remote = "https://e4ftl01.cr.usgs.gov/"),
                    sentinel = list(local = NA,
                                    remote = "missing"),
                    worldclim = list(local = NA,
                                     remote = "http://biogeo.ucdavis.edu/data/")
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
    packageStartupMessage(paste0("Hi, I am rasterTools ", packageVersion("rasterTools"), " and I help you with your spatial data!\nI don't know where your files are stored, please see '?setPaths' to create a suitable index."))
  } else{
    packageStartupMessage(paste0("Hi, I am rasterTools ", packageVersion("rasterTools"), " and I help you with your spatial data!"))

  }

  invisible()
}

# internal rasterTools data
globalVariables(c("rtPaths", "rtData", "rtTheme"))
# meta data
globalVariables(c("meta_modis", "meta_clc", "meta_emma", "meta_efta", "tiles_emma"))
# projections
globalVariables(c("projs"))