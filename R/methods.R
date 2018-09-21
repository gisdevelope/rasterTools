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
                       paste0(green(symbol$tick), yellow(" legend    "), " with values ordered ", ifelse(object@legend$ascending, "ascending", "descending"), " in ", object@legend$bins, " bins and a relative height of ", object@legend$sizeRatio, "\n",
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
#' @importFrom dplyr bind_cols
#' @importFrom raster extent
#' @export

setMethod(f = "getExtent",
          signature = "Spatial",
          definition = function(x){
            ext <- extent(x)
            bind_cols(x = c(ext@xmin, ext@xmax),
                      y = c(ext@ymin, ext@ymax))
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
          signature = c("geom", "numeric"),
          definition = function(x, subset){
            x@coords <- x@coords[subset,]
            ids <- unique(x@coords$id)
            x@attr <- x@attr[x@attr$id %in% ids,]
            return(x)
          })

#' @describeIn getSubset get a subset of the vertices of a \code{geom} based on a logical
#' @export

setMethod(f = "getSubset",
          signature = c("geom", "logical"),
          definition = function(x, subset){
            x@coords <- x@coords[which(subset),]
            ids <- unique(x@coords$id)
            x@attr <- x@attr[x@attr$id %in% ids,]
            return(x)
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