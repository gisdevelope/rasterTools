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
#' @details https://w3id.org/mtv/FISE/map-data-RPP/v0-3-2/internet/*
#'   https://w3id.org/mtv/FISE/map-data-RPP/v0-3-2/internet/Acer-campestre
#'   
#'   mention that values are integers and not "probabilities" between 0 and 1)
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
#'                  species = c("Quercus robur", "Alnus incana", "Betula sp"))
#' visualise(gridded = myTrees$`Betula sp`, trace = TRUE)
#' }
#' @importFrom checkmate testClass
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
  
  YGBl <- rtPalette(c("#fcf0d400", "#e8f5c3ff", "#bfe361ff", "#7abd2aff", "#438532ff", "#16301bff", "#050707ff", "#050707ff"), 
                    steps = c(5, 5, 20, 20, 20, 20, 10))
  rppCols <- c(YGBl(100), rep("#000000", 155))
  
  mhsCols <- c("#EAF3E6", "#E8F1E3", "#E6F0E1", "#E4EEDF", "#E3EDDD", "#E1ECDB",
               "#DFEAD9", "#DDE9D7", "#DBE8D4", "#DAE6D2", "#D8E5D0", "#D6E3CE",
               "#D4E2CC", "#D3E1CA", "#D1DFC8", "#CFDEC6", "#CEDDC3", "#CCDBC1",
               "#CADABF", "#C8D8BD", "#C6D7BB", "#C5D6B9", "#C3D4B7", "#C1D3B5",
               "#C0D2B2", "#BED0B0", "#BCCFAE", "#BACEAC", "#B9CCAA", "#B7CBA8",
               "#B5C9A6", "#B3C8A3", "#B2C7A1", "#B0C59F", "#AEC49D", "#ACC39B",
               "#ABC199", "#A9C097", "#A7BE95", "#A5BD92", "#A3BC90", "#A2BA8E",
               "#A0B98C", "#9EB88A", "#9DB688", "#9BB586", "#99B384", "#97B281",
               "#95B17F", "#94AF7D", "#92AE7B", "#90AD79", "#8FAB77", "#8DAA75",
               "#8BA972", "#89A770", "#87A66E", "#86A46C", "#84A36A", "#82A268",
               "#81A066", "#7F9F64", "#7D9E61", "#7B9C5F", "#799B5D", "#78995B",
               "#769859", "#749757", "#729555", "#719453", "#6F9350", "#6D914E",
               "#6C904C", "#6A8E4A", "#688D48", "#668C46", "#648A44", "#638941",
               "#61883F", "#5F863D", "#5D853B", "#5C8439", "#5A8237", "#588135",
               "#577F33", "#557E30", "#537D2E", "#517B2C", "#507A2A", "#4E7928",
               "#4C7726", "#4A7624", "#497422", "#47731F", "#45721D", "#43701B",
               "#416F19", "#406E17", "#3E6C15", "#3C6B13", "#3B6A11", rep("#000000", 154))
  
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
  efta_out <- NULL
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
    
    if(type == "rpp"){
      tempObject@legend@colortable <- rppCols
    } else{
      tempObject@legend@colortable <- mhsCols
    }
    
    names(tempObject) <- sub(thisSpecies, pattern = " ", replacement = "_")
    efta_out <- c(efta_out, setNames(list(tempObject), thisSpecies))
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

#' @describeIn oEFTA function to download data related to the EMMA dataset
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