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
#' @param inclMeta [\code{logical(1)}]\cr output merely the occurence data
#'   (\code{FALSE}, default), or output additionally metadata on the species
#'   (\code{TRUE})?
#' @return bla
#' @references de Rigo, D., Caudullo, G., Houston Durrant, T., San-Miguel-Ayanz,
#'   J., 2016. The European Atlas of Forest Tree Species: modelling, data and
#'   information on forest tree species. In: San-Miguel-Ayanz, J., de Rigo, D.,
#'   Caudullo, G., Houston Durrant, T., Mauri, A. (Eds.), European Atlas of
#'   Forest Tree Species. Publ. Off. EU, Luxembourg, pp.
#' @details https://w3id.org/mtv/FISE/map-data-RPP/v0-3-2/internet/*
#'   https://w3id.org/mtv/FISE/map-data-RPP/v0-3-2/internet/Acer-campestre
#' @family obtain operators
#' @examples
#'
#' # something
#' @importFrom checkmate testClass
#' @export

oEFTA <- function(mask = NULL, species = NULL, type = "rpp", inclMeta = FALSE){
  
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

  # go through 'species' to extract data
  efta <- NULL
  for(i in seq_along(species)){
    
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
  
  return(out)
  
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
    onlinePath <- rtPaths$efta$online
    message(paste0("  ... downloading the file from '", onlinePath, "'"))
    
    GET(url = paste0(onlinePath, file),
        write_disk(paste0(localPath, "/", file)),
        progress())
    
    message(paste0("  ... unzipping the files of '", file, "'"))
    unzip(paste0(localPath, "/", file), exdir = localPath)
  }
}