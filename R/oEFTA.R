#' Obtain data from the European Atlas of Forest Tree Species
#'
#' Obtain presence and habitat suitability
#' \href{http://forest.jrc.ec.europa.eu/european-atlas-of-forest-tree-species/atlas-data-and-metadata/}{data}
#' from the
#' \href{http://forest.jrc.ec.europa.eu/european-atlas-of-forest-tree-species/}{European
#' Atlas of Forest Tree Species}
#' @template mask
#' @param species [\code{character(.)}]\cr name(s) of species to get data for.
#'   Can be abbreviated if a \code{\link{catalog}} is provided.
#' @param type [\code{character(1)}]\cr the dataset type, either \code{"rpp"}
#'   (relative probability of presence) or \code{"mhs"} (maximum habitat
#'   suitability).
#' @return bla
#' @references de Rigo, D., Caudullo, G., Houston Durrant, T., San-Miguel-Ayanz,
#'   J., 2016. The European Atlas of Forest Tree Species: modelling, data and
#'   information on forest tree species. In: San-Miguel-Ayanz, J., de Rigo, D.,
#'   Caudullo, G., Houston Durrant, T., Mauri, A. (Eds.), European Atlas of
#'   Forest Tree Species. Publ. Off. EU, Luxembourg, pp.
#' @details The values in this dataset are originally stored as values in the
#'   range of 0 and 1 (the probability of presence). Here they are transformed
#'   to integer factors from 0 to 100.
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
#' # extract data for the derived mask
#' myTrees <- oEFTA(mask = myMask,
#'                  species = c("Quercus robur", "Alnus incana", 
#'                              "Pinus sylvestris", "Betula sp"))
#' visualise(gridded = myTrees, trace = TRUE)
#' }
#' @importFrom checkmate testClass
#' @importFrom raster crop unique stack
#' @export

oEFTA <- function(mask = NULL, species = NULL, type = "rpp"){
  
  # check arguments
  maskIsGeom <- testClass(mask, classes = "geom")
  maskIsSpatial <- testClass(mask, classes = "Spatial")
  assert(maskIsGeom, maskIsSpatial)
  speciesIsDF <- testDataFrame(species, any.missing = FALSE, ncols = 2, min.rows = 1, col.names = "named")
  if(speciesIsDF){
    assertNames(names(species), must.include = c("original", "abbr"))
    species <- species$original
  } else{
    assertCharacter(species)
  }
  assertSubset(type, choices = c("rpp", "mhs"))
  
  if(type == "rpp"){
    steps <- c(5, 5, 20, 20, 20, 20, 10)
    labels <- rep(c("marginal", "low", "mid-low", "medium", "mid-high", "high", "very-high"), steps)
    efta_palette <- rtPalette(colors = c("#fcf0d400", "#e8f5c3ff", "#bfe361ff", "#7abd2aff", "#438532ff", "#16301bff", "#050707ff", "#050707ff"), 
                              steps = steps)
    outCols <- c(efta_palette(100), rep("#000000", 155))
  } else{
    # steps <- c(, , , , , )
    # labels <- rep(c("negligible", "low", "mid-low", "medium", "mid-high", "high"), steps)
    # efta_palette <- rtPalette(colors = c("#", "#", "#", "#", "#", "#", "#", "#"), 
    #                           steps = steps)
    # outCols <- c(efta_palette(100), rep("#000000", 155))
  }
  
  # transform crs of the mask to the dataset crs
  if(maskIsSpatial){
    mask <- gFrom(input = mask)
  }
  targetCRS <- getCRS(x = mask)
  theExtent <- geomRectangle(anchor = getExtent(x = mask))
  theExtent <- setCRS(x = theExtent, crs = targetCRS)
  
  if(targetCRS != projs$laea){
    mask <- setCRS(x = mask, crs = projs$laea)
    targetExtent <- setCRS(theExtent, crs = projs$laea)
  } else{
    targetExtent <- theExtent
  }

  # determine the species that need to be dealt with
  if(any(!species %in% meta_efta$botanical)){
    warning(paste0("species '", species[which(!species %in% meta_efta$botanical)], "' is not part of this dataset"))
    species <- species[species %in% meta_efta$botanical]
  }
  if(type == "rpp"){
    dataExists <- meta_efta$rpp[meta_efta$botanical %in% species]
  } else{
    dataExists <- meta_efta$mhs[meta_efta$botanical %in% species]
  }
  
  # go through 'species' to extract data
  efta_out <- stack()
  for(i in seq_along(species)){
    thisSpecies <- species[i]
    fileName <- paste0(sub(thisSpecies, pattern = " ", replacement  = "-"), "_", type, ".tif")
    
    history <- list()
    message(paste0("I am handling the tree species '", thisSpecies, "':"))
    tempObject <- loadData(files = fileName, dataset = "efta")
    history <- c(history, paste0(tempObject[[1]]@history))
    
    message("  ... cropping to targeted study area")
    tempObject <- crop(tempObject, getExtent(x = targetExtent), snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
    history <-  c(history, list(paste0("object has been cropped")))
    
    # reproject
    if(getCRS(mask) != targetCRS){
      crs_name <- strsplit(targetCRS, " ")[[1]][1]
      message(paste0("  ... reprojecting to '", crs_name))
      tempObject <- setCRS(x = tempObject, crs = targetCRS)
      tempObject <- crop(tempObject, getExtent(x = theExtent), snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
      history <-  c(history, list(paste0("object has been reprojected to ", crs_name)))
    }
    tempObject <- round(tempObject*100)
    
    # create and set RAT table
    tempObject@data@isfactor <- TRUE
    ids <- unique(tempObject)
    if(type == "rpp"){
      tempObject@data@attributes <- list(data.frame(id = ids, presence = labels[ids+1]))
    } else{
      tempObject@data@attributes <- list(data.frame(id = ids, suitability = labels[ids+1]))
    }
    tempObject@history <- history
    
    # set colortable
    tempObject@legend@colortable <- outCols
    
    names(tempObject) <- sub(thisSpecies, pattern = " ", replacement = "_")
    efta_out <- stack(efta_out, tempObject)
  }

  # manage the bibliography entry
  bib <- bibentry(bibtype = "InBook",
                  title = "The European Atlas of Forest Tree Species: modelling, data and information on forest tree species",
                  author = c(person(given = "Daniele", family = "de Rigo"),
                             person(given = "Giovanni", family = "Caudullo"),
                             person(given = "Tracy", family = "Houston Durrant"),
                             person(given = "Jes\uFAs", family = "San-Miguel-Ayanz")
                  ),
                  chapter = 2,
                  year = 2016,
                  booktitle = "European Atlas of Forest Tree Species",
                  editor = c(person(given = "Jes\uFAs", family = "San-Miguel-Ayanz"),
                             person(given = "Daniele", family = "de Rigo"),
                             person(given = "Giovanni", family = "Caudullo"),
                             person(given = "Tracy", family = "Houston Durrant"),
                             person(given = "Achille", family = "Mauri")
                  ),
                  publisher = "Publ. Off. EU",
                  address = "Luxembourg"
  )
  
  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }
  
  return(efta_out)
  
}

#' @describeIn oEFTA function to download data related to the EFTA dataset
#' @param file [\code{character(1)}]\cr the name of the file to download.
#' @template localPath
#' @importFrom httr GET write_disk progress
#' @export

downloadEFTA <- function(file = NULL, localPath = NULL){
  
  assertCharacter(file, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(!is.null(localPath)){
    assertDirectory(localPath, access = "rw")
  }
  
  if(!is.null(file) & !is.null(localPath)){
    
    # https://w3id.org/mtv/FISE/map-data-RPP/v0-3-2/internet/Abies-alba
    # https://w3id.org/mtv/FISE/map-data-MHS/v0-3-2/internet/Abies-alba
    
    onlinePath <- rtPaths$efta$online
    message(paste0("  ... downloading the file from '", onlinePath, "'"))
    
    GET(url = paste0(onlinePath, file),
        write_disk(paste0(localPath, "/", file)),
        progress())
    
    message(paste0("  ... unzipping the files of '", file, "'"))
    unzip(paste0(localPath, "/", file), exdir = localPath)
  }
}