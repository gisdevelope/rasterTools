#' Obtain data from the Atlas of European Mammals.
#'
#' Obtain occurence data of mammals in Europe of the first EMMA
#' \href{https://www.european-mammals.org/}{dataset}.
#'
#' @template mask
#' @param species [\code{character(.)}]\cr name(s) of species to get occurrence
#'   for. Can be abbreviated if a \code{\link{catalog}} is provided.
#' @param version [\code{integerish(1)}]\cr The version of the \emph{Atlas of
#'   European Mammals}; the recent version is 1, but an update is in the making.
#' @param ... [various]\cr other arguments.
#' @details The website \href{http://www.european-mammals.org/}{Societas
#'   Europaea Mammalogica} kindly offers maps of the ocurrence of all mammals in
#'   Europe, which are parsed by \code{oEMMA}. See \code{\link{load_svg}} for a
#'   technical explanation.
#'
#'   The occurrence data are recorded and stored on a grid of 50 x 50 km. This
#'   is the same grid as used for the
#'   \href{http://www.luomus.fi/en/new-grid-system-atlas-florae-europaeae}{Atlas
#'   Florae Europaeae}.
#'
#'   The dataset \code{\link{meta_emma}} lists all available species.
#' @return A data-frame of the species of interest occuring in the area outlined
#'   by \code{mask}.
#' @references Mitchell-Jones A, Amori G, Bogdanowicz W, Kryštufek B, Reijnders
#'   P, Spitzenberger F, Stubbe M, Thissen J, Vohralík V and Zima J (1999). The
#'   Atlas of European Mammals. Academic Press, London
#' @family obtain operators
#' @examples
#' \dontrun{
#'
#' require(magrittr)
#'
#' mySpecies <- oEMMA(mask = rtGeoms$mask,
#'                    species = c("Apodemus agrarius",
#'                                "Apodemus flavicollis",
#'                                "Vulpes vulpes"))
#'
#' # Subset from a large set of files using an index
#' abbr_species <- function(x){
#'   tolower(paste(substr(unlist(strsplit(x, ' ')), 1, 3), collapse = '_'))
#' }
#' mySpecies <- catalog(path = rtPaths$emma$local,
#'                    type = 'svg', abbr = abbr_species) %>%
#'   subset(abbr %in% c("apo_agr", "apo_fla", "vul_vul")) %$%
#'   oEMMA(mask = myMask, species = original)
#'
#' }
#' @importFrom checkmate testClass testDataFrame assertNames testVector
#'   assertIntegerish assertLogical
#' @importFrom readr read_csv write_csv
#' @importFrom stringr str_replace
#' @importFrom dplyr bind_rows
#' @export

oEMMA <- function(mask = NULL, species = NULL, version = 1, ...){

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
  assertIntegerish(version, any.missing = FALSE, len = 1)

  species_dropout <- species[!species %in% meta_emma$species]
  species <- species[species %in% meta_emma$species]
  theSpecies <- str_replace(species, " ", "_")
  if(length(species_dropout) != 0){
    warning(paste0("species '", species_dropout, "' does not exist."))
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
  
  # determine tiles of interest
  # hier weiter
  tabEMMA <- getCoords(x = tiles_emma)
  tabMask <- getCoords(x = mask)
  ids <- unique(tabEMMA$id)
  xMatch <- yMatch <- NULL
  for(i in seq_along(ids)){
    temp <- tabEMMA[tabEMMA$id == ids[i],]
    xMatch <- c(xMatch, ifelse(any(tabMask$x < max(temp$x)) & any(tabMask$x > min(temp$x)), TRUE, FALSE))
    yMatch <- c(yMatch, ifelse(any(tabMask$y < max(temp$y)) & any(tabMask$y > min(temp$y)), TRUE, FALSE))
  }
  tiles <- xMatch & yMatch
  myTiles <- getSubset(x = tiles_emma, subset = tabEMMA$id %in% ids[tiles])
  tileNames <- as.character(getTable(myTiles)$square)

  # go through 'species' to extract data
  emma <- NULL
  for(i in seq_along(species)){

    blablabla(paste0("I am handling the species '", species[i]), ...)
    # check a csv-table already exists for that species. If it exists, we don't
    # have to read it in again and save some time.
    if(file.exists(paste0(rtPaths$emma$local, "/", theSpecies[i], ".csv"))){
      blablabla(paste0("  ... loading the file from '", rtPaths$emma$local, "'\n"), ...)
      allOcc <- read_csv(paste0(rtPaths$emma$local, "/", theSpecies[i], ".csv"), col_types = "ccc")
    } else{
      allOcc <- loadData(files = paste0(theSpecies[i], ".svg"),
                         dataset = "emma",
                         layer = "emma")
      write_csv(allOcc, paste0(rtPaths$emma$local, "/", theSpecies[i], ".csv"))
    }
    emma <- bind_rows(emma, allOcc[allOcc$square %in% tileNames,])

  }

  # manage the bibliography entry
  bib <- bibentry(bibtype = "Book",
                  title = "The Atlas of European Mammals",
                  author = c(person(given = "A J", family = "Mitchell-Jones"),
                             person(given = "Giovanni", family = "Amori"),
                             person(given = "W", family = "Bogdanowicz"),
                             person(given = "B", family = "Kry\u161tufek"),
                             person(given = "P J H", family = "Reijnders"),
                             person(given = "F", family = "Spitzenberger"),
                             person(given = "Michael", family = "Stubbe"),
                             person(given = "J M B", family = "Thissen"),
                             person(given = "V", family = "Vohral\uEDk"),
                             person(given = "J", family = "Zima")
                             ),
                  year = 1999,
                  publisher = "Academic Press",
                  address = "London"
                  )

  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  return(emma)
}

#' @describeIn oEMMA function to download data related to the EMMA dataset
#' @param file [\code{character(1)}]\cr the name of the file to download.
#' @template localPath
#' @importFrom httr GET write_disk progress
#' @importFrom stringr str_replace
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

downloadEMMA <- function(file = NULL, localPath = NULL){

  assertCharacter(file, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertDirectory(localPath, access = "rw")

  if(!is.null(file) & !is.null(localPath)){

    species <- strsplit(file, split = "[.]")[[1]][1]
    species <- str_replace(species, "_", "%20")
    onlinePath <- paste0(rtPaths$emma$remote, species)

    message(paste0("  ... downloading the file from '", onlinePath, "'"))

    GET(url = onlinePath,
        write_disk(paste0(localPath, "/", str_replace(file, " ", "_"))))
  }
}
