#' Obtain data of European mammals.
#'
#' Obtain occurence data of mammals in Europe of the first EMMA
#' \href{https://www.european-mammals.org/}{dataset}
#'
#' @param mask [\code{geom} | \code{SpatialPolygon*}]\cr spatial object of which
#'   the extent is the area of interest
#' @param species [\code{character(.)}]\cr name(s) of species to get occurrence
#'   for. Can be abbreviated if a \code{\link{catalog}} is provided.
#' @param version [\code{integerish(1)}]\cr The version of the \emph{Atlas of
#'   European Mammals}; the recent version is 1, but an update is in the making.
#' @param inclMeta [\code{logical(1)}]\cr output merely the occurence data
#'   (\code{FALSE}, default), or output additionally metadata on the species
#'   (\code{TRUE})?
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
#' @references  Mitchell-Jones A, Amori G, Bogdanowicz W, Kryštufek B, Reijnders
#'   P, Spitzenberger F, Stubbe M, Thissen J, Vohralík V and Zima J (1999). The
#'   Atlas of European Mammals. Academic Press, London
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
#' mySpecies <- oEMMA(mask = myMask,
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
#' @importFrom sp proj4string spTransform
#' @importFrom rgeos gIntersects gConvexHull
#' @export

oEMMA <- function(mask, species, version = 1, inclMeta = FALSE){

  # check arguments
  existsGeom <- testClass(mask, classes = "geom")
  existsSp <- testClass(mask, classes = "SpatialPolygon")
  existsSpDF <- testClass(mask, classes = "SpatialPolygonsDataFrame")
  existsSpatial <- ifelse(c(existsSp | existsSpDF), TRUE, FALSE)
  if(!existsGeom & !existsSpatial){
    stop("please provide either a SpatialPolygon* or a geom to mask with.")
  }
  isDFSpecies <- testDataFrame(species, any.missing = FALSE, ncols = 2, min.rows = 1, col.names = "named")
  if(isDFSpecies){
    assertNames(names(species), must.include = c("original", "abbr"))
    species <- species$original
  }
  isVectorSpecies <- testVector(species, strict = TRUE, min.len = 1, any.missing = FALSE)
  assertIntegerish(version, any.missing = FALSE, len = 1)
  assertLogical(inclMeta, any.missing = FALSE, len = 1)

  species_dropout <- species[!species %in% meta_emma$original]
  species <- species[species %in% meta_emma$original]
  if(length(species_dropout) != 0){
    warning(paste0("species '", species_dropout, "' does not exist."))
  }

  # transform crs of the mask to the dataset crs
  target_crs <- getCRS(x = mask)
  if(target_crs != projs$longlat){
    mask <- setCRS(x = mask, crs = projs$longlat)
  }

  # this will download the AFE grids and assemble them to an overal european grid
  message(paste0("I am handling the European AFE grid:"))
  downloadEMMA(getGrids = rtPaths$emma$gridLinks, localPath = rtPaths$emma$local)
  tiles_emma <- loadData(files = "cgrs_europe.kml",
                         localPath = rtPaths$emma$local)
  tiles_emma <- setCRS(x = tiles_emma, crs = projs$longlat)
  message("  ... done\n")
  if(existsGeom){
    mask <- gToSp(mask)
  }

  # determine relevant tiles according to 'mask'
  tiles <- as.logical(gIntersects(tiles_emma, gConvexHull(mask), byid = TRUE))
  myTiles <- tiles_emma[tiles,]
  tileNames <- levels(droplevels(myTiles$Name))

  # go through 'species' to extract data
  emma <- NULL
  for(i in seq_along(species)){

    message(paste0("I am handling the species '", species[i], "':"))
    # check a csv-table already exists for that species. If it exists, we don't
    # have to read it in again and save some time.
    if(file.exists(paste0(rtPaths$emma$local, "/", species[i], ".csv"))){
      message(paste0("  ... loading the file from '", rtPaths$emma$local, "'\n"))
      allOcc <- read.csv(paste0(rtPaths$emma$local, "/", species[i], ".csv"))
    } else{
      allOcc <- loadData(files = paste0(species[i], ".svg"),
                         dataset = "emma",
                         layer = "emma")
      write.csv(allOcc, paste0(rtPaths$emma$local, "/", species[i], ".csv"), row.names = FALSE)
    }
    emma <- rbind(emma, allOcc[allOcc$sq%in%tileNames,])
    row.names(emma) <- NULL

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

  toEnvironment(object = emma,
                name = paste0("emma_", species),
                envir = NULL)

}

#' @describeIn oEMMA function to download data related to the EMMA dataset
#' @param file [\code{character(1)}]\cr the name of the file to download.
#' @template localPath
#' @param getGrids [\code{character(1)}]\cr the online path to the gridfiles of
#'   the EMMA project; stored under \code{rtPaths$emma$gridLinks}
#' @param keepGrids [\code{logic(1)}]\cr should the the downloaded and seperate
#'   gridfiles be stored (\code{TRUE}), or should merely the overall European
#'   grid be stored (\code{FALSE}, default)?
#' @importFrom httr GET write_disk progress
#' @export

downloadEMMA <- function(file = NULL, localPath = NULL, getGrids = NULL,
                         keepGrids = FALSE){

  assertCharacter(file, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(!is.null(localPath)){
    assertDirectory(localPath, access = "rw")
  }
  assertCharacter(getGrids, pattern = "http://www.", any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(keepGrids, any.missing = FALSE, len = 1)

  if(!is.null(file) & !is.null(localPath)){

    species <- strsplit(file, split = "[.]")[[1]][1]
    species <- gsub(pattern = " ", replacement = "%20", x = species)
    onlinePath <- paste0(rtPaths$emma$online, species)

    message(paste0("  ... downloading the file from '", onlinePath, "'"))

    GET(url = onlinePath,
        write_disk(paste0(localPath, "/", file)),
        progress())

  } else if(!is.null(getGrids) & !is.null(localPath)){

    if(!file.exists(paste0(localPath, "/cgrs_europe.kml"))){
      GET(url = getGrids,
          write_disk(paste0(localPath, "/emma_grid_links.kml")))
      lines <- readLines(paste0(localPath, "/emma_grid_links.kml"))
      file.remove(paste0(localPath, "/emma_grid_links.kml"))
      entries <- unlist(lapply(seq_along(lines), function(x){
        line <- lines[x]
        pos <- regexpr("http:?.+(cgrs).+\\.kml", line)
        substr(line, pos, pos-1+attr(pos, "match.length"))
      }))
      entries <- entries[entries != ""]

      message("  ... downloading/processing the country grids\n")
      grids <- c('<?xml version="1.0" encoding="UTF-8"?>', '<kml xmlns="http://earth.google.com/kml/2.0">', '<Document>', '')
      pb <- txtProgressBar(min = 0, max = length(entries), style = 3, char=">", width=getOption("width")-14)

      for(i in seq_along(entries)){
        path <- entries[i]
        file <- strsplit(path, split = "/")[[1]]
        file <- file[length(file)]
        GET(url = path,
            write_disk(paste0(localPath, "/", file)))
        lines <- readLines(paste0(localPath, "/", file))
        grids <- c(grids, lines[grepl("Placemark", lines)])
        if(!keepGrids){
          file.remove(paste0(localPath, "/", file))
        }
        setTxtProgressBar(pb, i)
      }
      close(pb)

      grids <- c(grids, c('</Document>', '</kml>'))
      write(grids, file = paste0(localPath, "/cgrs_europe.kml"))
    }
  }
}