#' Get the table (generic)
#' @param x the object from which to derive the attribute table.
#' @param ... other arguments.
#' @export

setGeneric(name = "getTable",
           def = function(x, ...){
             standardGeneric("getTable")
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

#' Get elements or rows (generic)
#'
#' This function is equivalent to the \code{[} function on the
#' \code{@table} slot of a geom and extracts rows from this table.
#' @param x the object from which to get/extract an element.
#' @param row the row to extract.
#' @param ... other arguments.
#' @export

setGeneric(name = "getRow",
           def = function(x, row, ...){
             standardGeneric("getRow")
           })

#' Get variables or columns (generic)
#'
#' This function is equivalent to the \code{[} function on the
#' \code{@table} slot of a geom and extracts columns from this table.
#' @param x the object from which to get/extract an element.
#' @param column the column to extract.
#' @param ... other arguments.
#' @export

setGeneric(name = "getColumn",
           def = function(x, column, ...){
             standardGeneric("getColumn")
           })

#' Get subset (generic)
#'
#' Similar to \code{\link{getRow}}, \code{getSubset} returns the subsetted
#' \code{geom} and not only the selected rows.
#' @param x [\code{geom}]\cr object to \code{subset}.
#' @param subset [\code{integerish(.)} | \code{logical(.)}]\cr elements or rows
#'   to keep.
#' @param ... other arguments.
#' @export

setGeneric(name = "getSubset",
           def = function(x, subset, ...){
             standardGeneric("getSubset")
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
