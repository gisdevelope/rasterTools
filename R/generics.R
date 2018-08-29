#' Get the table (generic)
#' @param x the object from which to derive the attribute table.
#' @param ... other arguments.
#' @export

setGeneric(name = "getTable",
           def = function(x, ...){
             standardGeneric("getTable")
           })

#' Get the table of coordinates
#' @param x the object from which to extract the coordinates
#' @param ... other arguments.
#' @export

setGeneric(name = "getCoords",
           def = function(x, ...){
             standardGeneric("getCoords")
           })

#' Get the reference window (generic)
#' @param x the object from which to derive the reference window.
#' @param ... other arguments.
#' @export

setGeneric(name = "getWindow",
           def = function(x, ...){
             standardGeneric("getWindow")
           })

#' Get the extent (generic)
#' @param x the object from which to derive the extent.
#' @param ... other arguments.
#' @export

setGeneric(name = "getExtent",
           def = function(x, ...){
             standardGeneric("getExtent")
           })

#' Get subset (generic)
#'
#' \code{getSubset} returns the subsetted \code{geom}.
#' @param x [\code{geom}]\cr object to \code{subset}.
#' @param subset [\code{integerish(.)} | \code{logical(.)}]\cr elements or rows
#'   to keep.
#' @param ... other arguments.
#' @export

setGeneric(name = "getSubset",
           def = function(x, subset, ...){
             standardGeneric("getSubset")
           })

#' Get the coordinate reference system (generic)
#'
#' @param x the object from which to derive the coordinate reference system.
#' @param ... other arguments.
#' @seealso \code{\link{setCRS}}
#' @export

setGeneric(name = "getCRS",
           def = function(x, ...){
             standardGeneric("getCRS")
           })

#' Set (or transform) the coordinate reference system (generic)
#'
#' In case an object has not yet assigned a coordinate reference system, this
#' function simply assigns it. In case the object has already a valid crs, a
#' transformation to the new crs will be carried out. The transformation is
#' computed with the standard defined in the \code{rgdal} package.
#' @param x the object for which to set the coordinate reference system.
#' @param crs the coordinate reference system to set for this object.
#' @param ... other arguments.
#' @seealso \code{\link{getCRS}}
#' @export

setGeneric(name = "setCRS",
           def = function(x, crs, ...){
             standardGeneric("setCRS")
           })

#' Get the history (generic)
#' @param x the object from which to derive the history.
#' @param ... other arguments.
#' @export

setGeneric(name = "getHistory",
           def = function(x, ...){
             standardGeneric("getHistory")
           })
#
# setGeneric(name = "setWindow",
#            def = function(x, ...){
#              standardGeneric("setWindow")
#            })
