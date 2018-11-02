## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  oDATASET <- function(mask, ARG){
#  
#    maskIsGeom <- testClass(mask, classes = "geom")
#    maskIsSpatial <- testClass(mask, classes = "Spatial")
#    assert(maskIsGeom, maskIsSpatial)
#    assertXZY(ARG)
#  
#    datasetCRS <- projs$longlat

## ---- eval=FALSE---------------------------------------------------------
#    if(maskIsSpatial){
#      mask <- gFrom(input = mask)
#    }
#    targetCRS <- getCRS(x = mask)
#    theExtent <- geomRectangle(anchor = getExtent(x = mask))
#    theExtent <- setCRS(x = theExtent, crs = targetCRS)
#  
#    if(targetCRS != datasetCRS){
#      mask <- setCRS(x = mask, crs = datasetCRS)
#      targetExtent <- setCRS(theExtent, crs = datasetCRS)
#    } else{
#      targetExtent <- theExtent
#    }

## ---- eval=FALSE---------------------------------------------------------
#    aWindow <- data.frame(x = c(-180, 180),
#                          y = c(-60, 80))
#    datasetTiles <- geomTiles(window = aWindow, cells = c(36, 14), crs = datasetCRS)

## ---- eval=FALSE---------------------------------------------------------
#    tabTiles <- getTable(x = datasetTiles)
#    tabMask <- getTable(x = mask)
#    ids <- unique(tabTiles$id)
#    xMatch <- yMatch <- NULL
#    for(i in seq_along(ids)){
#      temp <- tabTiles[tabTiles$id == ids[i],]
#      xMatch <- c(xMatch, ifelse(any(tabMask$x < max(temp$x)) & any(tabMask$x > min(temp$x)), TRUE, FALSE))
#      yMatch <- c(yMatch, ifelse(any(tabMask$y < max(temp$y)) & any(tabMask$y > min(temp$y)), TRUE, FALSE))
#    }
#    tiles <- xMatch & yMatch
#    myTiles <- getSubset(tiles_gfc, tabTiles$id == ids[tiles])

## ---- eval=FALSE---------------------------------------------------------
#    tabTiles <- getTable(x = myTiles)
#    for (i in unique(tabTiles$id)){
#      min_x <- min(tabTiles$x[tabTiles$id == i])
#      max_y <- max(tabTiles$y[tabTiles$id == i])
#  
#      if(min_x < 0){
#        easting <- paste0(sprintf('%03i', abs(min_x)), 'W')
#      } else{
#        easting <-  paste0(sprintf('%03i', min_x), 'E')
#      }
#      if(max_y < 0){
#        northing <- paste0(sprintf('%02i', abs(max_y)), 'S')
#      } else{
#        northing <- paste0(sprintf('%02i', max_y), 'N')
#      }
#      gridId <- paste0(northing, '_', easting)
#      fileNames <-  paste0("Hansen_GFC2015_", layerNames, "_", gridId, '.tif')
#  
#      for(j in seq_along(ARG)){
#        # ---> Load and crop here <---
#      }
#    }

## ---- eval=FALSE---------------------------------------------------------
#      message(paste0("I am handling the gfc datasets with the grid ID '", gridId, "':\n"))
#      tempObject <- stack(loadData(files = fileNames, dataset = "gfc"))

## ---- eval=FALSE---------------------------------------------------------
#      targetExtent <- getExtent(x = targetExtent)
#      message("  ... cropping to targeted study area.\n")
#      tempObject <- crop(tempObject, targetExtent, snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")

## ---- eval=FALSE---------------------------------------------------------
#  

## ---- eval=FALSE---------------------------------------------------------
#      if(getCRS(mask) != targetCRS){
#        crs_name <- strsplit(targetCRS, " ")[[1]][1]
#        message(paste0("  ... reprojecting to '", crs_name))
#        tempObject <- setCRS(x = tempObject, crs = targetCRS)
#        theExtent <- getExtent(x = theExtent)
#        tempObject <- crop(tempObject, theExtent, snap = "out", datatype='INT1U', format='GTiff', options="COMPRESS=LZW")
#        history <-  c(history, list(paste0("object has been reprojected to ", crs_name)))
#      }

## ---- eval=FALSE---------------------------------------------------------
#  

## ---- eval=FALSE---------------------------------------------------------
#  load_shp <- function(path, layer){
#    rgdal::readOGR(dsn = path,
#                   layer = layer,
#                   verbose = FALSE)
#  }

## ---- eval=FALSE---------------------------------------------------------
#  load_FORMAT <- function(path, layer){
#    rgdal::readOGR(dsn = path,
#                   layer = layer,
#                   verbose = FALSE)
#  }

## ---- eval=FALSE---------------------------------------------------------
#      FILES <- list.files(PATH)
#      pb <- txtProgressBar(min = 0, max = length(FILES), style = 3, char=">", width=getOption("width")-14)
#      for(i in seq_along(FILES)){
#  
#        # store the name and an abbreviation of each file to a data-frame, then...
#        setTxtProgressBar(pb, i)
#  
#      }
#      close(pb)

## ---- eval=FALSE---------------------------------------------------------
#  bib <- bibentry(bibtype = "",
#                  title = "",
#                  author = person(""),
#                  year = ,
#                  ...
#  )

## ---- eval=FALSE---------------------------------------------------------
#  if(is.null(getOption("bibliography"))){
#    options(bibliography = bib)
#  } else{
#    currentBib <- getOption("bibliography")
#    if(!bib%in%currentBib){
#      options(bibliography = c(currentBib, bib))
#    }
#  }

