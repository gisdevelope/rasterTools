#' Determin number of vertices
#'
#' @param x [\code{geom}]\cr object from which to determine \code{length}

setMethod(f = "length",
          signature = "geom",
          definition = function(x){
            dim(x@table)[1]
          })

#' Print geom in the console
#'
#' @param object [\code{geom}]\cr object to \code{show}

setMethod(f = "show",
          signature = "geom",
          definition = function(object){
            cat("class        : ", class(object), "\n", sep = "")
            cat("feature type : ", object@type, "\n", sep = "")
            cat("features     : ", length(unique(object@table$id)), "  (", length(object), " vertices)\n", sep = "")
            cat("window       : ", min(object@window$x), ", ", max(object@window$x), ", ", min(object@window$y), ", ", max(object@window$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("extent       : ", min(object@table$x), ", ", max(object@table$x), ", ", min(object@table$y), ", ", max(object@table$y), "  (xmin, xmax, ymin, ymax)\n", sep = "")
            cat("scale        : ", object@scale, "\n", sep = "")
            cat("coord. ref.  : ", object@crs, "\n", sep = "")
            cat("variables    : ", length(object@table)-2, "  (", paste0(names(object@table)[!names(object@table) %in% c("x", "y")], collapse = ", "), ")\n", sep = "")
          })

#' @describeIn getTable get the attribute table (including coordinates) of a \code{geom}
#' @export

setMethod(f = "getTable",
          signature = "geom",
          definition = function(x){
            x@table
          })

#' @describeIn getTable get the attribute table (including coordinates) of a \code{RasterLayer}
#' @export

setMethod(f = "getTable",
          signature = "RasterLayer",
          definition = function(x){
            if(length(r@data@attributes) == 0){
              data.frame()
            } else{
              x@data@attributes[[1]]
            }
          })

#' @describeIn getWindow get the reference window of a \code{geom}
#' @export

setMethod(f = "getWindow",
          signature = "geom",
          definition = function(x){
            x@window
          })

# setMethod(f = "setWindow",
#           signature = "geom",
#           definition = function(x){
#
#           })

#' @describeIn getExtent get the bounding box of a \code{geom}
#' @export

setMethod(f = "getExtent",
          signature = "geom",
          definition = function(x){
            data.frame(x = c(min(x@table$x), max(x@table$x)),
                       y = c(min(x@table$y), max(x@table$y)))
          })

#' @describeIn getExtent get the bounding box of a \code{Raster*} object
#' @importFrom raster extent
#' @export

setMethod(f = "getExtent",
          signature = "Raster",
          definition = function(x){
            ext <- extent(x)
            data.frame(x = c(ext@xmin, ext@xmax),
                       y = c(ext@ymin, ext@ymax))
          })

#' @describeIn getExtent get the bounding box of a \code{Spatial*} object
#' @importFrom raster extent
#' @export

setMethod(f = "getExtent",
          signature = "Spatial",
          definition = function(x){
            ext <- extent(x)
            data.frame(x = c(ext@xmin, ext@xmax),
                       y = c(ext@ymin, ext@ymax))
          })

#' @describeIn getExtent get the bounding box of a \code{matrix} object
#' @export

setMethod(f = "getExtent",
          signature = "matrix",
          definition = function(x){
            data.frame(x = c(0, ncol(x)),
                       y = c(0, nrow(x)))
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

#' @describeIn getRow get a row of a \code{geom} based on a numeric
#' @export

setMethod(f = "getRow",
          signature = c("geom", "numeric"),
          definition = function(x, row){
            x@table[row,]
          })

#' @describeIn getRow get a row of a \code{geom} based on a logical
#' @export

setMethod(f = "getRow",
          signature = c("geom", "logical"),
          definition = function(x, row){
            x@table[which(row),]
          })

#' @describeIn getSubset get a subset of the vertices of a \code{geom} based on a numeric
#' @export

setMethod(f = "getSubset",
          signature = c("geom", "numeric"),
          definition = function(x, subset){
            x@table <- x@table[subset,]
            return(x)
          })

#' @describeIn getSubset get a subset of the vertices of a \code{geom} based on a logical
#' @export

setMethod(f = "getSubset",
          signature = c("geom", "logical"),
          definition = function(x, subset){
            x@table <- x@table[which(subset),]
            return(x)
          })

#' @describeIn getColumn get a column of the table of a \code{geom} base on a numeric
#' @export

setMethod(f = "getColumn",
          signature = c("geom", "numeric"),
          definition = function(x, column){
            x@table[,column]
          })

#' @describeIn getColumn get a column of the table of a \code{geom} base on a character
#' @export

setMethod(f = "getColumn",
          signature = c("geom", "character"),
          definition = function(x, column){
            x@table[,which(colnames(x@table) == column)]
          })

#' @describeIn getColumn get a column of the table of a \code{geom} base on a logical
#' @export

setMethod(f = "getColumn",
          signature = c("geom", "logical"),
          definition = function(x, column){
            x@table[,which(column)]
          })

#' @describeIn setCRS set the coordinate reference system of a \code{geom}
#' @export

setMethod(f = "setCRS",
          signature = "geom",
          definition = function(x, crs){
            if(is.na(x@crs)){
              x@crs <- crs
            } else{
              theCoords <- x@table[which(names(x@table) %in% c("x", "y"))]
              theWindow <- x@window
              if(x@crs != "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"){
                geographic <- rgdal::project(as.matrix(theCoords), proj = as.character(x@crs), inv = TRUE)
                geoWin <- rgdal::project(as.matrix(theWindow), proj = as.character(x@crs), inv = TRUE)
              } else{
                geographic <- as.matrix(theCoords)
                geoWin <- as.matrix(theWindow)
              }
              if(crs != "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"){
                projected <- rgdal::project(geographic, proj = as.character(crs))
                proWin <- rgdal::project(geoWin, proj = as.character(crs))
              } else{
                projected <- geographic
                proWin <- geoWin
              }
              x@table <- data.frame(projected, x@table[which(!names(x@table) %in% c("x", "y"))])
              x@crs <- crs
              x@window <- data.frame(proWin)
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

# res <- .Call("transform", proj4string(x), slot(CRSobj, "projargs"), n,
#              as.double(crds[,1]), as.double(crds[,2]), as.double(crds[,3]),
#              PACKAGE="rgdal")
#' @describeIn getHistory get the history of a \code{geom}
#' @export

setMethod(f = "getHistory",
          signature = "geom",
          definition = function(x){
            x@history
          })
