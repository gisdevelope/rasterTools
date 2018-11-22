#' @useDynLib rasterTools
#' @importFrom utils packageVersion data globalVariables

.onLoad <- function(...) {
  packageDataPath <- paste0(.libPaths()[1], "/rasterTools/data/")
  pathFile <- paste0(packageDataPath, "rtPaths.rda")

  if(!file.exists(pathFile)){
    rtPaths <- list(root = NA,
                    project = NA,
                    clc = list(local = NA,
                               remote = "missing"),
                    emma = list(local = NA,
                                remote = "https://www.european-mammals.org/php/rendermap.php?latname=",
                                gridLinks = "http://www.helsinki.fi/~rlampine/gmap/gridfilelinks.kml"),
                    efta = list(local = NA,
                                remote = "https://w3id.org/mtv/FISE/map-data"),
                    esalc = list(local = NA,
                                 remote = "ftp://geo10.elie.ucl.ac.be/v207/",
                                 qualityLayer = "https://storage.googleapis.com/cci-lc-v207/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7_Qualityflags.zip"),
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
    packageStartupMessage(paste0("Hi, I am rasterTools ", packageVersion("rasterTools"), " and I help you organise and compute spatial data!\nI don't know where your files are stored, please see '?setPaths' to create a suitable index."))
  } else{
    packageStartupMessage(paste0("Hi, I am rasterTools ", packageVersion("rasterTools"), " and I help you organise and compute spatial data!"))

  }

  invisible()
}

# internal rasterTools data
globalVariables(c("rtPaths", "rtData", "rtTheme", "rtMD5", "rtGeoms"))
# meta data
globalVariables(c("meta_modis", "meta_clc", "meta_emma", "meta_efta", "meta_esalc", "tiles_emma"))
# projections
globalVariables(c("projs"))