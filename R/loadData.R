#' Import spatial datasets
#'
#' Meta function to import, i.e. load or download and unpack spatial datasets
#' and other files containing spatial data.
#' @param files [\code{character(1)} | \code{list(.)}]\cr the file, as it is
#'   called in \code{localPath} or \code{onlinePath}. \code{files} can be a list
#'   to (down)load several files in the same directory/url with one call.
#'   \code{files} also accepts the (subsetted) output of \code{\link{catalog}},
#'   i.e. a \code{data.frame} with the two columns \code{original} and
#'   \code{abbr}.
#' @param dataset [\code{character(1)}]\cr character vector of the data dataset
#'   for which \code{files} should be imported; if this is given,
#'   \code{localPath} and \code{onlinePath} are overwritten with the internal
#'   dataset default (see \code{\link{updatePaths}}).
#' @template localPath
#' @param layer [\code{character(1)} | \code{integerish(1)}]\cr name or position
#'   of the layer, other than the \code{\link[rgdal]{readOGR}}-default, that
#'   should be loaded.
#' @param verbose [\code{logical(1)}]\cr should additional information be
#'   printed (\code{TRUE}) or should it be suppressed (\code{FALSE}, default)?
#' @details \code{importData} checks whether the required files are available in
#'   \code{localPath}. If nothing is found there, it attempts to download the
#'   files from \code{onlinePath}. \code{rasterTools} contains a predefined internal
#'   index, \code{getOption("rtPaths")}, where the local and online paths of
#'   the supported spatial datasets are stored. Please take a look at it and in
#'   case an online resource has been altered, you can adapt it here (see
#'   \code{\link{updatePaths}}).
#' @return a (list of) file(s). If there are several files of type
#'   \code{Raster*}, they are stacked.
#' @seealso The specific load operators: \code{\link{load_kml}},
#'   \code{\link{load_csv}}, \code{\link{load_kml}}, \code{\link{load_hdf}},
#'   \code{load_tif}, \code{load_dbf}, \code{load_shp}, \code{\link{load_svg}}
#' @examples
#' # load a batch of '.csv'-files containing coordinates into the gloabl environment.
#' myLocations <- loadData(files = c("aWindow.csv", "locations.csv"),
#'                         localPath = system.file("csv", package="rasterTools"), 
#'                         verbose = TRUE)
#' @importFrom checkmate testCharacter testDataFrame assertCharacter assertNames
#'   testIntegerish assertDirectory assertLogical assertEnvironment assertFile
#'   assertDataFrame
#' @importFrom utils file_test
#' @importFrom raster raster res<- stack
#' @export

# updatePaths(root = "/media/steffen/36551F673A1E43DF/spatial")

loadData <- function(files = NULL, dataset = NULL, layer = NULL, localPath = NULL,
                     verbose = FALSE){

  if(!is.null(files)){
    filesIsChar <- testCharacter(files, any.missing = FALSE, min.len = 1)
    filesIsDF <- testDataFrame(files, types = "character", ncols = 2, col.names = c("original", "abbr"))
    if(filesIsDF){
      files <- files$original
    }
  }
  assertCharacter(dataset, ignore.case = TRUE, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  if(!is.null(dataset) & is.null(localPath)){
    dataset <- tolower(dataset)
    localPath <- eval(parse(text = paste0("rtPaths$", dataset, "$local")))
  } else{
    if(is.null(localPath)) localPath <- getwd()
  }
  if(!is.null(layer)){
    layerIsInt <- testIntegerish(layer, len = 1, any.missing = FALSE)
    layerIsChar <- testCharacter(layer, len = 1, any.missing = FALSE)
  }
  assertDirectory(localPath, access = "rw")
  # asser (onlinePath)
  assertLogical(verbose, any.missing = FALSE, len = 1)

  testFiles <- function(files, path){
    filesInLocalPath <- list.files(path = path)
    filePaths <- list.files(path = path, full.names = TRUE)
    filesInLocalPath <- filesInLocalPath[file_test('-f', filePaths)]
    filePaths <- filePaths[file_test('-f', filePaths)]

    if(is.null(files)){
      # if 'files' is not defined, assume all files in 'localPath'
      # files <- filesInLocalPath
      fileExists <- rep(TRUE, times = length(files))
    } else{
      fileExists <- files %in% filesInLocalPath
      # filePaths <- filePaths[fileExists]
    }
    return(fileExists)
  }
  out <- list()

  # test how this function plays
  fileExists <- testFiles(files, path = localPath)
  # go through file and load each of them
  if(verbose){
    pb <- txtProgressBar(min = 0, max = length(files), style = 3, char=">", width = getOption("width")-14)
  }
  for(i in seq_along(files)){

    theFile <- files[i]
    thePath <- paste0(localPath, "/", theFile)
    fileTemp <- strsplit(theFile, split = "[.]")[[1]]
    fileType <- fileTemp[length(fileTemp)]
    fileName <- paste0(fileTemp[!fileTemp %in% fileType], collapse = ".")

    # if 'file' does not exist, attempt to download it (in case a downloadDATASET method is given)
    if(!fileExists[i]){
      if(is.null(dataset)){
        dataset <- "generic"
      }
      args <- list(file = files[i], localPath = localPath)

      do.call(what = paste0("download", toupper(dataset)),
              args = args)
    }

    fileExistsNow <- testFiles(theFile, path = localPath)

    if(fileExistsNow){

      # manage layer names
      if(is.null(layer)){
        layer <- fileName
      }

      # manage the arguments
      if(fileType %in% c("csv", "tif")){
        args <- list(path = thePath)
      } else{
        args <- list(path =  thePath,
                     layer = layer)
      }

      # and call the file type specific load_* function
      # message(paste0("  ... loading ", theFile, " from the local path '", localPath, "'\n"))
      history <- list()
      out_temp <- do.call(what = paste0("load_", fileType),
                          args = args)
      history <- c(history, paste0("object has been loaded from '", localPath, "'"))

      # assign history tag in case it's a 'Raster*'
      if(inherits(out_temp, "Raster")){
        out_temp@history <- history
      }

      out <- c(out, setNames(list(out_temp), fileName))

    } else{
      out <- c(message("file was not found locally or online"), setNames(list(out_temp), fileName))
    }
    if(verbose){
      setTxtProgressBar(pb, i)
    }

  }
  if(verbose){
    close(pb)
  }

  if(length(out) == 1){
    out <- out[[1]]
  }

  return(out)
}

#' Load \code{csv} files
#'
#' csv is the short form of \emph{Comma Separated Values}, which represents
#' simple tables of systematically delimited information. The files loaded with
#' this function should have the columns \code{x} and \code{y} and potentially
#' other columns that represent the attributes of these coordinates.
#'
#' This is a helper to \code{\link{loadData}} and is not intended to be used on
#' its own.
#' @template path
#' @return a \code{point geometry} of the coordinates
#' @family loaders
#' @export

load_csv <- function(path){

  assertFile(path, access = "r", extension = "csv")
  out <- read.csv(path, stringsAsFactors = FALSE)
  assertDataFrame(out, types = "numeric", min.cols = 3)
  colnames(out) <- tolower(colnames(out))
  assertNames(names(out), permutation.of = c("x", "y", "id"))

  out <- new(Class = "geom",
             type = "point",
             table = data.frame(x = out$x, y = out$y, id = out$id),
             window = data.frame(x = rep(c(min(out$x), max(out$x)), each = 2), y = c(min(out$y), max(out$y), max(out$y), min(out$y))),
             scale = "absolute",
             crs = as.character(NA),
             history = list(paste0("geom has been loaded from ", path)))

  return(out)
}

#' Load \code{kml} files
#'
#' kml is the short form of \emph{Keyhole Markup Language}, which uses the
#' \code{XML} format; this is a simple wrapper around \link[rgdal]{readOGR}.
#'
#' This is a helper to \code{\link{loadData}} and is not intended to be used on
#' its own.
#' @template path
#' @param layer [\code{character(1)}]\cr the layer name.
#' @return the \code{readOGR} standard for kml files.
#' @family loaders
#' @importFrom rgdal readOGR
#' @export

load_kml <- function(path, layer){

  assertFile(path, access = "r", extension = "kml")
  assertCharacter(layer, ignore.case = TRUE, any.missing = FALSE)

  readOGR(dsn = path,
          layer = layer,
          verbose = FALSE)

}

#' Load \code{hdf} files
#'
#' hdf is the short form of \emph{Hierarchical Data Format}, which is a
#' standardised file format to store scientific data. There are two commonly
#' used version of this format, hdf4 and hdf5. For now, \code{load_hdf} loads
#' only files of format hdf5. It is a wrapper of \link[gdalUtils]{gdalinfo}.
#'
#' This is a helper to \code{\link{loadData}} and is not intended to be used on
#' its own.
#' @template path
#' @param layer [\code{character(1)}]\cr the layer name.
#' @return \code{RasterLayer} of the loaded \code{hdf} file
#' @family loaders
#' @importFrom gdalUtils gdalinfo
#' @importFrom raster stack raster
#' @export

load_hdf <- function(path, layer){
  # get the layers; could be done via get_subdatasets(path), but if we look into
  # get_subdatasets(), we see that it is a rather low-level wrapper around
  # gdalinfo(). So we can also use gdalinfo() and have more control.
  assertFile(path, access = "r", extension = "hdf")

  gdalinfo_raw <- gdalinfo(path)

  if(!any(grep("SUBDATASET", gdalinfo_raw))){
    stop(paste0("The dataset in ", path, " has not been given according to the expected specifications. I abort loading it."))
  } else{

    subsets_name <- gdalinfo_raw[grep(glob2rx("*SUBDATASET*NAME*"), gdalinfo_raw)]
    subset_desc <- gdalinfo_raw[grep(glob2rx("*SUBDATASET*DESC*"), gdalinfo_raw)]

    paths <- unlist(lapply(
      seq_along(subsets_name), function(i){
        gsub("\"", "", strsplit(subsets_name[i], "=")[[1]][2])
      }
    ))
  }

  # select the layer(s)
  if(layer %in% seq_along(files)){
    layer <- layer
  } else if(!is.numeric(layer)){
    layer <- grep(layer, files, ignore.case = TRUE)
    if(length(layer) == 0){
      message(paste0("      -> you did not specify any layer, so I create a RasterStack\n"))
      layer <- seq_along(files)
    }
  }
  tsds <- paths[layer]
  files <- files[layer]

  if(length(tsds)>1){
    out <- raster::stack(tsds)
  } else{
    out <- raster::raster(tsds)
  }
  names(out) <- files

  return(out)
}

#' Load \code{svg} files
#'
#' svg is the short form of \emph{Scalable Vector Graphics}. These files are
#' basically text files interpreted and consequently visualised by any program
#' that is suitable to do so (such as your web-browser or
#' \href{www.inkscape.org}{inkscape}). This means that everything that is
#' visible in the image, is somehow coded into the text. Consequently, in case
#' the file is systematic and contains information of interest, these can be
#' turned into computer readable data.
#'
#' This is a helper to \code{\link{loadData}} and is not intended to be used on
#' its own.
#' @template path
#' @param layer [\code{character(1)}]\cr the origin of the dataset that is
#'   captured by the file. Recently this is only \code{"emma"}.
#' @return \code{data.frame} of the content of interest depending on
#'   \code{layer}.
#' @family loaders
#' @export

load_svg <- function(path, layer){

  assertFile(path, access = "r", extension = "svg")
  assertCharacter(layer, ignore.case = TRUE, any.missing = FALSE)

  message(paste0("  ... loading the file from '", path, "'\n"))
  txt <- suppressWarnings(readLines(path))

  # throw out uninteresting lines
  if(layer == "emma"){

    path <- strsplit(path, split = "/")[[1]]
    species <- path[length(path)]
    species <- strsplit(species, split = "[.]")[[1]][1]

    txt <- txt[grep("use id[[:space:]]?=", txt)]
    txt <- gsub("'", "", txt)
    txt <- gsub('\"', "", txt)
    txt <- sub('<use id[[:space:]]?=[[:space:]]?', "", txt)
    txt <- sub('xlink:href[[:space:]]?=[[:space:]]?#', "", txt)
    txt <- sub('[[:space:]]?/>', "", txt)
    txt <- gsub(' = ', "=", txt)

    # make a proper data.frame out of the mess.
    allOcc <- strsplit(txt, " ")
    allOcc <- as.data.frame(do.call(rbind, allOcc))[-c(2:3)] # these values which look like coordinates are merely values needed to render the svg file.
    allOcc <- cbind(species, allOcc)
    colnames(allOcc) <- c("species", "square", "year")

    return(allOcc)
  } else{
    stop("loading svg files other than from the EMMA dataset has not been programmed yet...")
  }
}


load_dbf <- function(path){

  foreign::read.dbf(path)

}

load_shp <- function(path, layer){

  rgdal::readOGR(dsn = path,
                 layer = layer,
                 verbose = FALSE)

}

load_tif <- function(path){

  raster::raster(path)

}

