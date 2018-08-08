#' Obtain MODIS data
#'
#' Obtain data from various MODIS \href{https://modis.gsfc.nasa.gov/}{datasets}.
#' @template mask
#' @param period [\code{integerish(.)}]\cr year of interest (YYYY), optionally
#'   concatenated to day of the year (DDD), which must be between \code{001} and
#'   \code{366}.
#' @param product [\code{character(1)}]\cr name of the modis-product of
#'   interest. Can be abbreviated if an \code{index} is provided.
#' @param layer [\code{character(1)} | \code{integerish(1)}]\cr layer(s) of
#'   interest. Either the integer of the layers position or the name of the
#'   layer.
#' @param version [\code{integerish(1)} | \code{character(1)}]\cr version of a
#'   modis dataset with 3 leading zeros, e.g. 055, default = "newest".
#' @param process [\code{logical(1)}]\cr should the data be procesed further in
#'   R (\code{TRUE}, default) or should the data merely be downloaded and stored
#'   on the harddisc (\code{FALSE})?
#' @param raw [\code{logical}]\cr should the raw modis-data be obtained
#'   (\code{TRUE}) or should a correction be applied (\code{FALSE}, default).
#'   Applying the correction (i.e. the range is cut to the valid range and the
#'   correction factor is applied) is highly recommended, to end up with
#'   consisten data.
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
                   version = "newest", process = TRUE, raw = FALSE){

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
  assertCharacter(product)
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
  if(version == "newest"){
    productVersion <- sprintf('%03i', meta$newest_version[1])
  } else{
    assertIntegerish(version, len = 3)
    productVersion <- sprintf('%03i', version)
  }
  availableTiles <- c("h00v08", "h00v09", "h00v10", 
                      "h01v07", "h01v08", "h01v09", "h01v10", "h01v11", 
                      "h02v06", "h02v08", "h02v09", "h02v10", "h02v11", 
                      "h03v06", "h03v07", "h03v09", "h03v10", "h03v11", 
                      "h04v09", "h04v10", "h04v11", 
                      "h05v10", "h05v11", "h05v13", 
                      "h06v03", "h06v11", 
                      "h07v03", "h07v05", "h07v06", "h07v07", 
                      "h08v03", "h08v04", "h08v05", "h08v06", "h08v07", "h08v08", "h08v09", 
                      "h09v02", "h09v03", "h09v04", "h09v05", "h09v06", "h09v07", "h09v08", "h09v09", 
                      "h10v02", "h10v03", "h10v04", "h10v05", "h10v06", "h10v07", "h10v08", "h10v09", "h10v10", "h10v11", 
                      "h11v02", "h11v03", "h11v04", "h11v05", "h11v06", "h11v07", "h11v08", "h11v09", "h11v10", "h11v11", "h11v12", 
                      "h12v01", "h12v02", "h12v03", "h12v04", "h12v05", "h12v07", "h12v08", "h12v09", "h12v10", "h12v11", "h12v12", "h12v13", 
                      "h13v01", "h13v02", "h13v03", "h13v04", "h13v08", "h13v09", "h13v10", "h13v11", "h13v12", "h13v13", "h13v14", 
                      "h14v01", "h14v02", "h14v03", "h14v04", "h14v09", "h14v10", "h14v11", "h14v14", "h14v16", "h14v17", 
                      "h15v01", "h15v02", "h15v03", "h15v05", "h15v07", "h15v11", "h15v14", "h15v15", "h15v16", "h15v17", 
                      "h16v00", "h16v01", "h16v02", "h16v05", "h16v06", "h16v07", "h16v08", "h16v09", "h16v12", "h16v14", "h16v16", "h16v17", 
                      "h17v00", "h17v01", "h17v02", "h17v03", "h17v04", "h17v05", "h17v06", "h17v07", "h17v08", "h17v10", "h17v12", "h17v13", "h17v15", "h17v16", "h17v17", 
                      "h18v00", "h18v01", "h18v02", "h18v03", "h18v04", "h18v05", "h18v06", "h18v07", "h18v08", "h18v09", "h18v14", "h18v15", "h18v16", "h18v17", 
                      "h19v00", "h19v01", "h19v02", "h19v03", "h19v04", "h19v05", "h19v06", "h19v07", "h19v08", "h19v09", "h19v10", "h19v11", "h19v12", "h19v15", "h19v16", "h19v17", 
                      "h20v01", "h20v02", "h20v03", "h20v04", "h20v05", "h20v06", "h20v07", "h20v08", "h20v09", "h20v10", "h20v11", "h20v12", "h20v13", "h20v15", "h20v16", "h20v17", 
                      "h21v01", "h21v02", "h21v03", "h21v04", "h21v05", "h21v06", "h21v07", "h21v08", "h21v09", "h21v10", "h21v11", "h21v13", "h21v15", "h21v16", "h21v17", 
                      "h22v01", "h22v02", "h22v03", "h22v04", "h22v05", "h22v06", "h22v07", "h22v08", "h22v09", "h22v10", "h22v11", "h22v13", "h22v14", "h22v15", "h22v16", 
                      "h23v01", "h23v02", "h23v03", "h23v04", "h23v05", "h23v06", "h23v07", "h23v08", "h23v09", "h23v10", "h23v11", "h23v15", "h23v16", 
                      "h24v02", "h24v03", "h24v04", "h24v05", "h24v06", "h24v07", "h24v12", "h24v15", 
                      "h25v02", "h25v03", "h25v04", "h25v05", "h25v06", "h25v07", "h25v08", "h25v09", 
                      "h26v02", "h26v03", "h26v04", "h26v05", "h26v06", "h26v07", "h26v08", 
                      "h27v03", "h27v04", "h27v05", "h27v06", "h27v07", "h27v08", "h27v09", "h27v10", "h27v11", "h27v12", "h27v14", 
                      "h28v03", "h28v04", "h28v05", "h28v06", "h28v07", "h28v08", "h28v09", "h28v10", "h28v11", "h28v12", "h28v13", "h28v14", 
                      "h29v03", "h29v05", "h29v06", "h29v07", "h29v08", "h29v09", "h29v10", "h29v11", "h29v12", "h29v13", 
                      "h30v05", "h30v06", "h30v07", "h30v08", "h30v09", "h30v10", "h30v11", "h30v12", "h30v13", 
                      "h31v06", "h31v07", "h31v08", "h31v09", "h31v10", "h31v11", "h31v12", "h31v13", 
                      "h32v07", "h32v08", "h32v09", "h32v10", "h32v11", "h32v12", 
                      "h33v07", "h33v08", "h33v09", "h33v10", "h33v11", 
                      "h34v07", "h34v08", "h34v09", "h34v10", 
                      "h35v08", "h35v09", "h35v10")

  # put 'period' into the right format "c(from YYYYDDD, to YYYYDDD)"
  leap_year <- seq(from = 1904, to = 2196, by = 4)
  if(length(period) == 1){
    period <- rep(period, 2)
  }
  if(nchar(period[1])!=7){
    period[1] <- paste0(period[1], "001")
  }
  if(nchar(period[2])!=7){
    if(period[2] %in% leap_year){
      period[2] <- paste0(period[2], "366")
    } else{
      period[2] <- paste0(period[2], "365")
    }
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
  myTiles <- getSubset(tiles_modis, tabMODIS$id %in% ids[tiles])

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
    
    # not all tiles contain actual data, skip those that don't
    if(!gridId %in% availableTiles){
      next
    }

    modis_out <- list()
    # go through all files, which are available according to the arguments, for the tile
    allObjects <- NULL

    for(j in seq_along(validDates)){

      productFiles <- downloadMODIS(getFiles = paste0(rtPaths$modis$online, product, ".", productVersion, "/", validDates[j], "/"))
      validFiles <- productFiles[grep(gridId, productFiles)]

      history <- list()
      message(paste0("I am handling the modis product '", product, "' with the grid ID '", gridId, "' for ", validDates[j], ":"))

      
      if(!process){
        downloadMODIS(file = validFiles,
                      localPath = paste0(rtPaths$modis$local, "/", product))
        next
      } else{
        tempObject <- loadData(files = validFiles,
                               dataset = "modis",
                               layer = layer,
                               localPath = paste0(rtPaths$modis$local, "/", product))
      }

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
      # names(tempObject) <- paste0(tempNames, "_", validDates[j])

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

    # check also whether the file is available locally already
    filePieces <- strsplit(file, split = "[.]")[[1]]
    pathPieces <- strsplit(localPath, split = "/")[[1]]
    product <- pathPieces[length(pathPieces)]
    date <- substr(filePieces[[2]], 2, nchar(filePieces[[2]]))
    date <- doyToDate(year = substr(date, 1, 4), doy = substr(date, 5, 7))
    date <- strftime(x = date, format = "%Y.%m.%d")
    version <- filePieces[[4]]

    onlinePath <- paste0(rtPaths$modis$online, product, ".", version, "/", date)
    message(paste0("  ... downloading the file from '", onlinePath, "'"))
    usr <- tryCatch(get("usr"), error = function(e) NULL)
    pwd <- tryCatch(get("usr"), error = function(e) NULL)
    if(is.null(usr)){
      usr <- readline("earthdata.nasa.gov user name: ")
      assign("usr", usr, envir = .GlobalEnv)
    }
    if(is.null(pwd)){
      pwd <- readline("earthdata.nasa.gov password: ")
      assign("pwd", pwd, envir = .GlobalEnv)
    }

    GET(url = paste0(onlinePath, "/", file),
        authenticate(usr, pwd),
        write_disk(paste0(localPath, "/", file), overwrite = TRUE),
        progress())
    
    # http://www.spatial-analyst.net/wiki/index.php?title=Download_and_resampling_of_MODIS_images

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
