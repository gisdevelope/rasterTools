#' Obtain MODIS data
#'
#' Obtain data from various MODIS \href{https://modis.gsfc.nasa.gov/}{datasets}.
#' @template mask
#' @param period [\code{integerish(.)}]\cr year of interest (YYYY), optionally
#'   concatenated to day of the year (DDD), which must be between \code{001} and
#'   \code{366}.
#' @param product name of the modis-product of interest. Can be abbreviated if
#'   an \code{index} is provided.
#' @param layer layer(s) of interest. Can be numerical or a character string of
#'   its name.
#' @param version version of a modis dataset with 3 leading zeros, e.g. 055,
#'   default = "newest".
#' @param raw logical; should the raw modis-data be obtained (\code{TRUE}) or
#'   should a correction be applied (\code{FALSE}, default). Applying the
#'   correction (i.e. the range is cut to the valid range and the correction
#'   factor is applied) is highly recommended, to end up with consisten data.
#' @details The period can either be given as a single value if only one year is
#'   of interest, or as two values, the start and end date. If you do not
#'   specify DDD, it will be set to "001" and "366" for start and end year
#'   respectively.
#'
#'   Supported MODIS-products - in the sense that predefined meta-data are
#'   available within this package -  are: \itemize{ \item{MOD09A1: Surface
#'   Reflectance, 8 day, 500 meter, February 2000 - present} \item{MOD09Q1:
#'   Surface Reflectance, 8 day, 250 meter, February 2000 - present}
#'   \item{MOD11A2: Land Surface Temperature/Emissivity, 8 day, 1000 meter,
#'   March 2000 - present} \item{MOD13Q1: Vegetation Indices, 16 day, 250 meter,
#'   February 2000 - present} \item{MOD13A1: Vegetation Indices, 16 day, 500
#'   meter, February 2000 - present} \item{MOD13A2: Vegetation Indices, 16 day,
#'   1000 meter, February 2000 - present} \item{MOD13A3: Vegetation Indices,
#'   monthly, 1000 meter, February 2000 - present} \item{MOD14A2: Thermal
#'   Anomalies and Fire, 8 day, 1000 meter, February 2000 - present}
#'   \item{MOD15A2H: Leaf Area Index/FPAR, 8 day, 500 meter, July 2002 -
#'   present} \item{MOD17A2H: Gross Primary Productivity, 8 day, 500 meter,
#'   February 2000 - present} \item{MOD17A3H: Net Primary Production, yearly,
#'   500 meter, 2000 - present} \item{MOD44W: Land-Water Mask, yearly, 250
#'   meter, 2000 - 2015} }
#'
#'   Get additional information about the spatial background from
#'   \href{https://code.env.duke.edu/projects/mget/wiki/SinusoidalMODIS}{Marine
#'   Geospatial Ecology Tools}
#' @return A \code{RasterLayer} or \code{RasterStack} of MODIS data.
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
#' water <- oMODIS(mask = myMask, period = 2000,
#'                 product = "MOD44W", layer = 1)
#' visualise(gridded = water, trace = TRUE)
#'
#' # not specifying 'layer' results in a stack of all layers
#' myMODIS <- oMODIS(mask = myMask, period = 2012,
#'                   product = "MOD17A3")
#' visualise(gridded = myMODIS, trace = TRUE)
#'
#' modEvi <- oMODIS(mask = myMask, period = c(2016100, 2016250),
#'                  product = "MOD13A3", layer = "EVI")
#' visualise(gridded = modEvi, trace = TRUE)
#'
#' # get the (updated) bibliography
#' reference(style = "bibtex")
#' }
#' @importFrom sp proj4string spTransform bbox
#' @importFrom rgeos gIntersects gConvexHull gBuffer
#' @importFrom raster values setValues crop projectRaster stack mosaic
#' @export

oMODIS <- function(mask = NULL, period = NULL, product = NULL, layer = NULL,
                   version = "newest", raw = FALSE){

  # check arguments
  existsGeom <- testClass(mask, classes = "geom")
  existsSp <- testClass(mask, classes = "SpatialPolygon")
  existsSpDF <- testClass(mask, classes = "SpatialPolygonsDataFrame")
  existsSpatial <- ifelse(c(existsSp | existsSpDF), TRUE, FALSE)
  if(!existsGeom & !existsSpatial){
    stop("please provide either a SpatialPolygon* or a geom to mask with.")
  }
  assertIntegerish(period, any.missing = FALSE, min.len = 1, max.len = 2, unique = TRUE)
  assertTRUE(nchar(period[1]) %in% c(4, 7))
  assertTRUE(nchar(period[2]) %in% c(NA, 4, 7))
  assertCharacter(product, fixed = "MOD")
  layerIsInt <- testIntegerish(layer, any.missing = FALSE, min.len = 1)
  layerIsChar <- testCharacter(layer, any.missing = FALSE, min.len = 1, ignore.case = TRUE)
  assertLogical(raw)

  # load meta data
  meta <- meta_modis[grep(paste0("(?i)", product, "(?-i)$"), meta_modis$product),]
  if(layerIsInt){
    meta <- meta[layer,]
  } else if(layerIsChar){
    meta <- meta[grep(layer, meta$sds_layer_name, ignore.case = TRUE),]
  }
  if(nrow(meta)==0){
    return(c("'product' does not match any dataset of modis or is undefined."))
  }

  # check version
  if(version=="newest"){
    productVersion <- sprintf('%03i', meta$newest_version[1])
  } else{
    productVersion <- sprintf('%03i', version)
  }

  # put 'period' into the right format "c(from YYYYDDD, to YYYYDDD)"
  if(length(period) == 1){
    period <- rep(period, 2)
  }
  if(nchar(period[1])!=7){
    period[1] <- paste0(period[1], "001")
  }
  if(nchar(period[2])!=7){
    period[2] <- paste0(period[2], "366")
  }

  # set the valid dates for this product
  year <- substr(period, 0, 4)
  doy <- substr(period, 5, nchar(period))
  theDates <- doyToDate(year, doy)

  # transform crs of the mask to the dataset crs
  target_crs <- getCRS(x = mask)
  if(target_crs != projs$sinu){
    mask <- setCRS(x = mask, crs = projs$sinu)
  }
  theExtent <- getExtent(x = mask)
  if(existsSpatial){
    mask <- gFrom(mask)
  }

  # create the tiles geometry to determine the data-subset to load
  modWindow <- data.frame(x = c(-20015109.354, 20015109.354),
                          y = c(-10007554.677, 10007554.677))
  tiles_modis <- geomTiles(window = modWindow, cells = c(36, 18), crs = projs$sinu)

  # determine tiles of interest
  tabMODIS <- getTable(tiles_modis)
  tabMask <- getTable(mask)
  ids <- unique(tabMODIS$id)
  xMatch <- yMatch <- NULL
  for(i in seq_along(ids)){
    temp <- tabMODIS[tabMODIS$id == ids[i],]
    xMatch <- c(xMatch, ifelse(any(tabMask$x < max(temp$x)) & any(tabMask$x > min(temp$x)), TRUE, FALSE))
    yMatch <- c(yMatch, ifelse(any(tabMask$y < max(temp$y)) & any(tabMask$y > min(temp$y)), TRUE, FALSE))
  }
  tiles <- xMatch & yMatch
  myTiles <- getSubset(tiles_modis, tabMODIS$id == ids[tiles])

  productDates <- downloadMODIS(getDates = paste0(rtPaths$modis$online, product, ".", productVersion, "/"))
  productDates <- as.POSIXlt(productDates, format = "%Y.%m.%d", tz = "GMT")
  validDates <- strftime(x = productDates[productDates < theDates[2] & productDates > theDates[1]-1], format = "%Y.%m.%d")

  # create a matrix that matches the tile names
  nameMat <- matrix(nrow = 36, ncol = 18, byrow = F, data = c(648:1))
  nameMat <- t(apply(nameMat, 2, rev))
  colnames(nameMat) <- c(0:35); rownames(nameMat) <- c(0:17)

  modis <- list()
  tabTiles <- getTable(myTiles)
  # go through all tiles
  for(i in seq_along(unique(tabTiles$id))){

    tileID <- unique(tabTiles$id)[i]

    # determine gridId to narrow down the files
    colRowNames <- which(nameMat==tileID, arr.ind = T)-1
    gridId <- paste0(c("h", "v"), sprintf('%02i', as.integer(rev(colRowNames))), collapse = "")

    modis_out <- list()
    # go through all files, which are available according to the arguments, for the tile
    allObjects <- NULL

    for(j in seq_along(validDates)){

      productFiles <- downloadMODIS(getFiles = paste0(rtPaths$modis$online, product, ".", productVersion, "/", validDates[j], "/"))
      validFiles <- productFiles[grep(gridId, productFiles)]

      history <- list()
      message(paste0("I am handling the modis product '", product, "' with the grid ID '", gridId, "' for ", validDates[j], ":\n"))

      tempObject <- loadData(files = validFiles,
                             dataset = "modis",
                             layer = layer,
                             localPath = paste0(rtPaths$modis$local, "/", product))

      # usually the corrected values are preferable
      if(!raw){
        for(k in 1:dim(tempObject)[3]){
          oldVals <- values(tempObject[[k]])
          newVals <- oldVals * meta$correction_factor[k]
          newVals[newVals < meta$valid_min[k] | newVals > meta$valid_max[k]] <- NA
          tempObject <- setValues(tempObject, newVals, layer = k)
          tempNames <- as.character(meta$description)
        }
      }
      history <- c(history, paste0(tempObject@history, " with the grid ID '", gridId, "'"))

      message("  ... cropping to targeted study area\n")
      tempObject <- crop(tempObject, theExtent, snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
      history <-  c(history, list(paste0("object has been cropped")))

      # reproject
      if(getCRS(mask) != target_crs){
        crs_name <- strsplit(target_crs, " ")[[1]][1]
        message(paste0("  ... reprojecting to '", crs_name, "'\n"))
        mask <- setCRS(x = mask, crs = target_crs)
        tempObject <- setCRS(tempObject, crs = target_crs, method = "ngb", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
        theExtent <- getExtent(x = mask)
        tempObject <- crop(tempObject, theExtent, snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
        history <-  c(history, list(paste0("object has been reprojected to ", crs_name)))
      }
      tempObject@history <- history
      names(tempObject) <- paste0(tempNames, "_", validDates[j])

      if(is.null(allObjects)){
        allObjects <- tempObject
      } else{
        allObjects <- suppressWarnings(stack(list(allObjects, tempObject)))
      }
    }

    modis_out <- c(modis_out, setNames(list(allObjects), gridId))
  }

  if(length(modis_out) > 1){

    message("the mask intersects with two tiles, I merge them now ...\n")
    modis_name <- names(modis_out)
    modis_list_names <- names(modis_out[[1]])
    modis_out <- lapply(
      seq_along(modis_out[[1]]), function(i){
        x <- unlist(lapply(modis_out, "[", i))
        names(x) <- NULL
        do.call(mosaic, c(x, fun = "mean"))
      }
    )
    modis_out <- setNames(modis_out, modis_list_names)

  } else{
    modis_out <- modis_out[[1]]
  }

  # manage the bibliography entry
  bib <- eval(parse(text = paste0("ref_modis$", product)))

  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  return(modis_out)
}

#' @describeIn oMODIS function to download data related to the MODIS products
#' @param file [\code{character(1)}]\cr the name of the file to download.
#' @template localPath
#' @param getDates [\code{character(1)}]\cr the online path to the directory
#'   where folders for all valid dates are stored (recently the first level
#'   after selecting the product).
#' @param getFiles [\code{character(1)}]\cr the online path to the directory
#'   where product files are stored.
#' @importFrom httr authenticate write_disk progress content GET
#' @export

downloadMODIS <- function(file = NULL, localPath = NULL, getDates = NULL,
                          getFiles = NULL){

  assertCharacter(file, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(!is.null(localPath)){
    assertDirectory(localPath, access = "rw")
  }
  assertCharacter(getDates, pattern = "https://.", any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(getFiles, pattern = "https://.", any.missing = FALSE, len = 1, null.ok = TRUE)

  if(!is.null(file) & !is.null(localPath)){

    filePieces <- strsplit(file, split = "[.]")[[1]]
    pathPieces <- strsplit(localPath, split = "/")[[1]]
    product <- pathPieces[length(pathPieces)]
    date <- substr(filePieces[[2]], 2, nchar(filePieces[[2]]))
    date <- doyToDate(year = substr(date, 1, 4), doy = substr(date, 5, 7))
    date <- strftime(x = date, format = "%Y.%m.%d")
    version <- filePieces[[4]]

    onlinePath <- paste0(rtPaths$modis$online, product, ".", version, "/", date)
    message(paste0("  ... downloading the file from '", onlinePath, "'"))
    usr <- readline("earthdata.nasa.gov user name: ")
    pwd <- readline("earthdata.nasa.gov password: ")

    GET(url = paste0(onlinePath, "/", file),
        authenticate(usr, pwd),
        write_disk(paste0(localPath, "/", file)),
        progress())

  } else if(!is.null(getDates)){

    productDates <- content(GET(url = getDates), as = "text")
    lines <- strsplit(productDates, split = "\n")[[1]]
    entries <- unlist(lapply(seq_along(lines), function(x){
      line <- lines[x]
      pos <- regexpr("[[:digit:]]+.[[:digit:]]+.[[:digit:]]+\\/", line)
      substr(line, pos, pos-1+attr(pos, "match.length"))
    }))
    entries <- entries[entries != ""]
    entries <- substr(entries, 0, nchar(entries)-1)

    return(entries)

  } else if(!is.null(getFiles)){

    productFiles <- content(GET(url = getFiles), as = "text")
    lines <- strsplit(productFiles, split = "\n")[[1]]
    entries <- unlist(lapply(seq_along(lines), function(x){
      line <- lines[x]
      pos <- regexpr("[[:alnum:]]+.[[:alnum:]]+.[[:alnum:]]+.[[:digit:]]+.[[:digit:]]+.hdf\\\"", line)
      substr(line, pos, pos-2+attr(pos, "match.length"))
    }))
    entries <- entries[entries != ""]

    return(entries)

  }
}
