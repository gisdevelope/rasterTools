#' Determin number of vertices
#'
#' @param x [\code{geom}]\cr object from which to determine \code{length}.

setMethod(f = "length",
          signature = "geom",
          definition = function(x){
            dim(x@coords)[1]
          })

#' Print geom in the console
#'
#' @param object [\code{geom}]\cr object to \code{show}.

setMethod(f = "show",
          signature = "geom",
          definition = function(object){
            cat("class      : ", class(object), "\n", sep = "")
            cat("type       : ", object@type, "\n", sep = "")
            cat("features   : ", length(unique(object@coords$fid)), "  (", length(object), " vertices)\n", sep = "")
            cat("window     : ", min(object@window$x), ", ", max(object@window$x), ", ", min(object@window$y), ", ", max(object@window$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("extent     : ", min(object@coords$x), ", ", max(object@coords$x), ", ", min(object@coords$y), ", ", max(object@coords$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("scale      : ", object@scale, "\n", sep = "")
            cat("crs        : ", object@crs, "\n", sep = "")
            cat("attributes : ", length(object@attr), "  (", paste0(names(object@attr)[!names(object@attr) %in% c("x", "y")], collapse = ", "), ")\n", sep = "")
          })

#' Print rtTheme in the console
#'
#' @param object [\code{rtTheme}]\cr object to \code{show}.
#' @importFrom crayon green yellow red cyan
#' @importFrom cli symbol

setMethod(f = "show",
          signature = "rtTheme",
          definition = function(object){
            cat(ifelse(object@title$plot, 
                       paste0(green(symbol$tick), yellow(" title    "), " in ", object@title$colour, " with fontsize ", object@title$fontsize), 
                       paste0(red(symbol$cross), yellow(" title    "))), "\n")
            cat(ifelse(object@box$plot,
                       paste0(green(symbol$tick), yellow(" box      "), " in ", object@box$colour, " with ", object@box$linewidth, " wide ", object@box$linetype, " lines"),
                       paste0(red(symbol$cross), yellow(" box      "))),"\n")
            cat(ifelse(object@xAxis$plot,
                       paste0(green(symbol$tick), yellow(" xAxis    "), " with ", object@xAxis$bins, " bins and a margin of ", object@xAxis$margin, "\n",
                              ifelse(object@xAxis$label$plot,
                                     paste0(green(symbol$tick), yellow("  - label  "), "'", object@xAxis$label$title, "' in ", object@xAxis$label$colour, " with fontsize ", object@xAxis$label$fontsize, ifelse(object@xAxis$label$rotation != 0, paste0(" and a rotation of ", object@xAxis$label$rotation), "")),
                                     paste0(red(symbol$cross), yellow("  - label  "))), "\n",
                              ifelse(object@xAxis$ticks$plot,
                                     paste0(green(symbol$tick), yellow("  - ticks  "), "in ", object@xAxis$ticks$colour, " with fontsize ", object@xAxis$ticks$fontsize, " rounded to ", object@xAxis$ticks$digits, ifelse(object@xAxis$ticks$digits == 1, " digit", " digits")),
                                     paste0(red(symbol$cross), yellow("  - ticks  ")))),
                       paste0(red(symbol$cross), yellow(" xAxis    "))), "\n")
            cat(ifelse(object@yAxis$plot,
                       paste0(green(symbol$tick), yellow(" yAxis    "), " with ", object@yAxis$bins, " bins and a margin of ", object@yAxis$margin, "\n",
                              ifelse(object@yAxis$label$plot,
                                     paste0(green(symbol$tick), yellow("  - label  "), "'", object@yAxis$label$title, "' in ", object@yAxis$label$colour, " with fontsize ", object@yAxis$label$fontsize, ifelse(object@yAxis$label$rotation != 0, paste0(" and a rotation of ", object@yAxis$label$rotation), "")),
                                     paste0(red(symbol$cross), yellow("  - label  "))), "\n",
                              ifelse(object@yAxis$ticks$plot,
                                     paste0(green(symbol$tick), yellow("  - ticks  "), "in ", object@yAxis$ticks$colour, " with fontsize ", object@yAxis$ticks$fontsize, " rounded to ", object@yAxis$ticks$digits, ifelse(object@yAxis$ticks$digits == 1, " digit", " digits")),
                                     paste0(red(symbol$cross), yellow("  - ticks  ")))),
                       paste0(red(symbol$cross), yellow(" yAxis    "))), "\n")
            cat(ifelse(object@grid$plot,
                       paste0(green(symbol$tick), yellow(" grid     "), " in ", object@grid$colour, " with ", object@grid$linewidth, " wide ", object@grid$linetype, " lines"),
                       paste0(red(symbol$cross), yellow(" grid     "))), "\n")
            cat(ifelse(object@legend$plot,
                       paste0(green(symbol$tick), yellow(" legend   "), " with values ordered ", ifelse(object@legend$ascending, "ascending", "descending"), " in ", object@legend$bins, " bins and a relative height of ", object@legend$sizeRatio, "\n",
                              ifelse(object@legend$label$plot,
                                     paste0(green(symbol$tick), yellow("  - label  "), "in ", object@legend$label$colour, " with fontsize ", object@legend$label$fontsize),
                                     paste0(red(symbol$cross), yellow("  - label  "))), "\n",                              
                              ifelse(object@legend$box$plot,
                                     paste0(green(symbol$tick), yellow("  - box    "), "in ", object@legend$box$colour, " with ", object@legend$box$linewidth, " wide ", object@legend$box$linetype, " lines"),
                                     paste0(red(symbol$cross), yellow("  - box    ")))),
                       paste0(red(symbol$cross), yellow(" legend    "))), "\n")
            cat(paste0(green(symbol$tick), yellow(" geom     "), " with ", object@geom$scale$x, "-colour scaled to ", cyan(object@geom$scale$to), ", ", object@geom$linewidth, " wide ", object@geom$linetype, " lines and ", object@geom$pointsize, " wide points of type ", object@geom$pointsymbol, "\n"))
            cat(paste0(green(symbol$tick), yellow(" raster   "), " with colours scaled to ", cyan(object@raster$scale)))
          })

#' @describeIn getTable get the attribute table of a \code{geom}
#' @importFrom tibble as_tibble
#' @export

setMethod(f = "getTable",
          signature = "geom",
          definition = function(x){
            as_tibble(x@attr)
          })

#' @describeIn getTable get the attribute table of a \code{RasterLayer}
#' @importFrom tibble tibble as_tibble
#' @export

setMethod(f = "getTable",
          signature = "RasterLayer",
          definition = function(x){
            if(length(x@data@attributes) == 0){
              tibble()
            } else{
              as_tibble(x@data@attributes[[1]])
            }
          })

#' @describeIn getTable get the attribute table of a \code{Spatial*} object
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export

setMethod(f = "getTable",
          signature = "Spatial",
          definition = function(x){
            
            theData <- NULL
            sourceClass <- class(x)[1]
            prev <- 0
            
            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame", "SpatialPixels", "SpatialPixelsDataFrame")){
              type <- "point"
              
              if(sourceClass %in% "SpatialPointsDataFrame"){
                theData <- tibble(fid = seq_along(x@coords[,1]), n = 1)
                theData <- bind_cols(theData, x@data)
              } else{
                theData <- tibble(fid = seq_along(x@coords[,1]), n = 1)
              }

            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
              type <- "point"
              
              for(i in seq_along(x@coords)){
                
                if(sourceClass %in% "SpatialMultiPointsDataFrame"){
                  tempData <- tibble(i, length(x@coords[[i]][,1]), x@data[i,])
                  theData <- bind_rows(theData, tempData)
                  otherNames <- colnames(x@data)
                } else{
                  tempData <- tibble(i, length(x@coords[[i]][,1]))
                  theData <- bind_rows(theData, tempData)
                  otherNames <- NULL
                }
              }
              colnames(theData) <- c("fid", "n", otherNames)

            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
              type <- "line"
              
              for(i in seq_along(x@lines)){
                theLines <- x@lines[[i]]
                
                for(j in seq_along(theLines@Lines)){
                  if(sourceClass %in% "SpatialLinesDataFrame"){
                    tempData <- tibble(prev + j, dim(theLines@Lines[[j]]@coords)[1], x@data[i,])
                    theData <- bind_rows(theData, tempData)
                    otherNames <- colnames(x@data)
                  } else{
                    theData <- bind_rows(theData, tibble(prev + j, dim(theLines@Lines[[j]]@coords)[1]))
                    otherNames <- NULL
                  }
                }
                prev <- prev + length(theLines@Lines)
                
              }
              colnames(theData) <- c("fid", "n", otherNames)

            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialGrid", "SpatialGridDataFrame")){
              type <- "polygon"
              
              for(i in seq_along(x@polygons)){
                thePolys <- x@polygons[[i]]
                
                for(j in seq_along(thePolys@Polygons)){
                  if(sourceClass %in% "SpatialPolygonsDataFrame"){
                    tempData <- tibble(prev + j, dim(thePolys@Polygons[[j]]@coords)[1], x@data[i,])
                    theData <- bind_rows(theData, tempData)
                    otherNames <- colnames(x@data)
                  } else{
                    theData <- bind_rows(theData, tibble(prev + j, dim(thePolys@Polygons[[j]]@coords)[1]-1))
                    otherNames <- NULL
                  } 
                }
                prev <- prev + length(thePolys@Polygons)
                
              }
              colnames(theData) <- c("fid", "n", otherNames)

            }
            return(theData)
          })

#' @describeIn getTable get the attribute table of a \code{sf} object
#' @importFrom tibble tibble as_tibble
#' @export

setMethod(f = "getTable",
          signature = "sf",
          definition = function(x){
            fids <- NULL
            for(i in 1:dim(x)[1]){
              fids <- c(fids, length(x$geometry[[i]]))
            }
            temp <- tibble(fid = 1:dim(x)[1],
                           n = fids)
            bind_cols(temp, as_tibble(x))
          })

#' @describeIn setTable set the attribute table of a \code{geom}
#' @importFrom dplyr left_join
#' @export

setMethod(f = "setTable",
          signature = "geom",
          definition = function(x, table){
            stopifnot(is.data.frame(table))
            stopifnot(any(names(table) %in% "fid"))
            nIDs <- length(x@attr$fid)
            x@attr <- left_join(x@attr, table)
            return(x)
          })

#' @describeIn setTable set the attribute table of a \code{RasterLayer}
#' @importFrom raster ratify
#' @export

setMethod(f = "setTable",
          signature = "RasterLayer",
          definition = function(x, table){
            stopifnot(is.data.frame(table))
            temp <- ratify(x)
            nIDs <- length(temp@data@attributes[[1]][,1])
            stopifnot(dim(table)[1] == nIDs)
            temp@data@attributes <- list(table)
            return(temp)
          })

#' @describeIn getCoords get the table of coordinates of a \code{geom}
#' @importFrom tibble as_tibble
#' @export

setMethod(f = "getCoords",
          signature = "geom",
          definition = function(x){
            as_tibble(x@coords)
          })

#' @describeIn getCoords get the table of coordinates of a \code{Spatial*} object
#' @importFrom tibble tibble as_tibble
#' @export

setMethod(f = "getCoords",
          signature = "Spatial",
          definition = function(x){
            
            theCoords <- NULL
            prev <- 0
            sourceClass <- class(x)[1]
            if(sourceClass %in% c("SpatialGrid")){
              x <- as(x, "SpatialPolygons")
            } else if(sourceClass %in% "SpatialGridDataFrame"){
              x <- as(x, "SpatialPolygonsDataFrame")
            } else if(sourceClass %in% "SpatialPixels"){
              x <- as(x, "SpatialPoints")
            } else if(sourceClass %in% "SpatialPixelsDataFrame"){
              x <- as(x, "SpatialPointsDataFrame")
            }
            sourceClass <- class(x)[1]
            
            if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame")){
              
              theCoords <- bind_cols(vid = seq_along(x@coords[,1]), 
                                     fid = seq_along(x@coords[,1]),
                                     as_tibble(x@coords))
              colnames(theCoords) <- c("vid", "fid", "x", "y")
              
            } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
              
              for(i in seq_along(x@coords)){
                tempCoords <- data.frame(fid = i,
                                         vid = seq_along(x@coords[[i]][,1]),
                                         x = x@coords[[i]][,1],
                                         y = x@coords[[i]][,2])
                theCoords <- rbind(theCoords, tempCoords)
              }
              
            } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
              
              for(i in seq_along(x@lines)){
                theLines <- x@lines[[i]]
                for(j in seq_along(theLines@Lines)){
                  theLine <- theLines@Lines[[j]]
                  
                  tempCoords <- tibble(fid = prev + j,
                                       vid = seq_along(theLine@coords[,1]),
                                       x = theLine@coords[,1],
                                       y = theLine@coords[,2])
                  theCoords <- bind_rows(theCoords, tempCoords)
                }
                prev <- prev + length(theLines@Lines)
              }
              
            } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
              
              for(i in seq_along(x@polygons)){
                thePolys <- x@polygons[[i]]
                for(j in seq_along(thePolys@Polygons)){
                  thePoly <- thePolys@Polygons[[j]]
                  
                  if(thePoly@hole){
                    fidID <- j-1
                  } else{
                    fidID <- j
                  }
                  tempCoords <- tibble(fid = prev + j,
                                       vid = seq_along(thePoly@coords[,1]),
                                       x = thePoly@coords[,1],
                                       y = thePoly@coords[,2])
                  theCoords <- bind_rows(theCoords, tempCoords)
                }
                prev <- prev + length(thePolys@Polygons)
              }
              
            }
            
            return(theCoords)
          })

#' @describeIn getCoords get the table of coordinates of a \code{sf} object
#' @importFrom tibble as_tibble
#' @importFrom sf st_geometry_type st_coordinates
#' @export

setMethod(f = "getCoords",
          signature = "sf",
          definition = function(x){
            
            sourceClass <- st_geometry_type(x)
            temp <- st_coordinates(x)
            if(sourceClass %in% c("POINT", "MULTIPOINT")){
              
              theCoords <- tibble(fid = seq_along(temp[, 1]),
                                  vid = seq_along(temp[, 1]), 
                                  x = temp[,1],
                                  y = temp[,2])

            } else if(sourceClass %in% c("LINESTRING", "MULTILINESTRING")){
              
              theCoords <- tibble(fid = temp[,3],
                                  vid = seq_along(temp[, 1]), 
                                  x = temp[,1],
                                  y = temp[,2])
              
            } else if(sourceClass %in% c("POLYGON", "MULTIPOLYGON")){

              fids <- temp[,c(3:4)]
              prev <- 0
              tempFids <- NULL
              for(i in seq_along(unique(fids[,2]))){
                tempFids <- c(tempFids, fids[fids[,2] == i,][,1] + prev)
                prev <- max(tempFids)
              }
              theCoords <- tibble(fid = tempFids,
                                  vid = seq_along(temp[, 1]), 
                                  x = temp[,1],
                                  y = temp[,2])
            
            }
            return(theCoords)
          })

#' @describeIn getWindow get the reference window of a \code{geom}
#' @importFrom tibble as_tibble
#' @export

setMethod(f = "getWindow",
          signature = "geom",
          definition = function(x){
            as_tibble(x@window)
          })

#' @describeIn setWindow set the reference window of a \code{geom}
#' @export

setMethod(f = "setWindow",
          signature = "geom",
          definition = function(x, to){
            stopifnot(all(c("x", "y") %in% colnames(to)))
            if(nrow(to) == 4){
              x@window <- to[c("x", "y")]
            } else if(nrow(to) == 2){
              x@window <- data.frame(x = rep(to$x, each = 2),
                                     y = c(to$y, rev(to$y)))
            } else{
              stop("no suitable window provided.")
            }
            return(x)
          })

#' @describeIn getExtent get the bounding box of a \code{geom}
#' @importFrom dplyr bind_cols
#' @export

setMethod(f = "getExtent",
          signature = "geom",
          definition = function(x){
            bind_cols(x = c(min(x@coords$x), max(x@coords$x)),
                      y = c(min(x@coords$y), max(x@coords$y)))
          })

#' @describeIn getExtent get the bounding box of a \code{Raster*} object
#' @importFrom dplyr bind_cols
#' @importFrom raster extent
#' @export

setMethod(f = "getExtent",
          signature = "Raster",
          definition = function(x){
            ext <- extent(x)
            bind_cols(x = c(ext@xmin, ext@xmax),
                      y = c(ext@ymin, ext@ymax))
          })

#' @describeIn getExtent get the bounding box of a \code{Spatial*} object
#' @importFrom tibble tibble
#' @importFrom raster extent
#' @export

setMethod(f = "getExtent",
          signature = "Spatial",
          definition = function(x){
            ext <- extent(x)
            tibble(x = c(ext@xmin, ext@xmax),
                   y = c(ext@ymin, ext@ymax))
          })

#' @describeIn getExtent get the bounding box of a \code{sf} object
#' @importFrom tibble tibble
#' @importFrom sf st_bbox
#' @export

setMethod(f = "getExtent",
          signature = "sf",
          definition = function(x){
            ext <- st_bbox(x)
            tibble(x = c(ext[[1]], ext[[3]]),
                   y = c(ext[[2]], ext[[4]]))
          })

#' @describeIn getExtent get the bounding box of a \code{matrix} object
#' @importFrom dplyr bind_cols
#' @export

setMethod(f = "getExtent",
          signature = "matrix",
          definition = function(x){
            bind_cols(x = c(0, ncol(x)),
                      y = c(0, nrow(x)))
          })

#' @describeIn getSubset get a subset of the vertices of a \code{geom} based on a numeric
#' @export

setMethod(f = "getSubset",
          signature = c("geom"),
          definition = function(x, attr, coords){
            if(!missing(attr)){
              if(is.logical(attr)){
                stopifnot(dim(x@attr)[1] == length(attr))
                matches <- attr
              } else if(is.integer(attr)){
                matches <- attr
              } else if(is.character(attr)){
                matches <- eval(parse(text = attr), envir = x@attr)
              }
              x@attr <- x@attr[matches,]
              x@coords <- x@coords[x@coords$fid %in% x@attr$fid,]
            }
            if(!missing(coords)){
              if(is.logical(coords)){
                stopifnot(dim(x@coords)[1] == length(coords))
                matches <- coords
              } else if(is.integer(coords)){
                matches <- coords
              } else if(is.character(coords)){
                matches <- eval(parse(text = coords), envir = x@coords)
              }
              x@coords <- x@coords[matches,]
              x@attr <- x@attr[x@attr$fid %in% x@coords$fid]
              
              nVids <- sapply(unique(x@coords$fid), function(i){
                length(x@coords$vid[x@coords$fid == i])
              })
              x@attr$n <- nVids
            }
            return(x)
          })

#' @describeIn getSubset get a subset of the vertices of a \code{sf} object based on a logical
#' @export

setMethod(f = "getSubset",
          signature = c("sf", "logical"),
          definition = function(x, attr, coords){
            x[attr,]
          })

#' @describeIn getCRS get the coordinate reference system of a \code{geom}
#' @export

setMethod(f = "getCRS",
          signature = "geom",
          definition = function(x){
            x@crs
          })

#' @describeIn getCRS get the coordinate reference system of a \code{Raster*} object
#' @export

setMethod(f = "getCRS",
          signature = "Raster",
          definition = function(x){
            as.character(x@crs)
          })

#' @describeIn getCRS get the coordinate reference system of a \code{Spatial*} object
#' @export

setMethod(f = "getCRS",
          signature = "Spatial",
          definition = function(x){
            as.character(x@proj4string)
          })

#' @describeIn getCRS get the coordinate reference system of a \code{sf} object
#' @importFrom sf st_crs
#' @export

setMethod(f = "getCRS",
          signature = "sf",
          definition = function(x){
            st_crs(x)$proj4string
          })

#' @describeIn setCRS set the coordinate reference system of a \code{geom}
#' @importFrom stringr str_split
#' @export

setMethod(f = "setCRS",
          signature = "geom",
          definition = function(x, crs){
            if(is.na(x@crs)){
              x@crs <- crs
            } else{
              theCoords <- x@coords[which(names(x@coords) %in% c("x", "y"))]
              if(!all(c("+proj=longlat", "+ellps=WGS84") %in% str_split(x@crs, " ")[[1]])){
                geographic <- rgdal::project(as.matrix(theCoords), proj = as.character(x@crs), inv = TRUE)
              } else{
                geographic <- as.matrix(theCoords)
              }
              if(crs != "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"){
                projected <- rgdal::project(geographic, proj = as.character(crs))
              } else{
                projected <- geographic
              }
              x@coords <- data.frame(projected, x@coords[which(!names(x@coords) %in% c("x", "y"))])
              x@crs <- crs
              x <- setWindow(x = x, to = getExtent(x))
            }
            return(x)
          })

#' @describeIn setCRS set the coordinate reference system of a \code{Raster*} object
#' @importFrom raster crs projectRaster
#' @export

setMethod(f = "setCRS",
          signature = "Raster",
          definition = function(x, crs, ...){
            if(is.na(x@crs)){
              x@crs <- crs(crs)
            } else{
              x <- projectRaster(from = x, crs = crs, ...)
            }
            return(x)
          })

#' @describeIn setCRS set the coordinate reference system of a \code{Spatial*} object
#' @importFrom raster crs
#' @importFrom sp spTransform
#' @export

setMethod(f = "setCRS",
          signature = "Spatial",
          definition = function(x, crs){
            if(is.na(x@proj4string)){
              x@proj4string <- crs(crs)
            } else{
              x <- spTransform(x, CRSobj = crs(crs))
            }
            return(x)
          })

#' @describeIn setCRS set the coordinate reference system of a \code{sf} object
#' @importFrom raster crs
#' @importFrom sp spTransform
#' @export

setMethod(f = "setCRS",
          signature = "sf",
          definition = function(x, crs){

          })

#' @describeIn getHistory get the history of a \code{geom}
#' @export

setMethod(f = "getHistory",
          signature = "geom",
          definition = function(x){
            x@history
          })

#' @describeIn getHistory get the history of a \code{RasterLayer}
#' @export

setMethod(f = "getHistory",
          signature = "RasterLayer",
          definition = function(x){
            x@history
          })

#' @describeIn getHistory get the history of a \code{RasterBrick}
#' @export

setMethod(f = "getHistory",
          signature = "RasterBrick",
          definition = function(x){
            hist <- list()
            for(i in 1:dim(x)[3]){
              hist <- c(hist, x[[i]]@history)
            }
            return(hist)
          })

#' @describeIn getHistory get the history of a \code{RasterStack}
#' @export

setMethod(f = "getHistory",
          signature = "RasterStack",
          definition = function(x){
            hist <- list()
            for(i in 1:dim(x)[3]){
              hist <- c(hist, x[[i]]@history)
            }
            return(hist)
          })