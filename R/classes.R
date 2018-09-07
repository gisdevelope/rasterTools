#' Geometry class and methods
#'
#' A \code{geom} stores the vertices that outline the shape and all additional
#' information that characterise a feature. A \code{geom} can be spatial, but
#' does not have to be. A \code{geom} can either have absolute or relative
#' values, where relative values specify the vertex position relative to the
#' \code{window} slot.
#'
#' A \code{geom} either has the feature type \itemize{ \item \code{"point"},
#' when none of the vertices are connected to other vertices, \item
#' \code{"line"}, when vertices with the same id are connected according to
#' their order without the line closing in itself and \item \code{"polygon"}
#' according to the same definition, except that the line closes in itself.}
#' Moreover, a \code{geom} does NOT have the slot \emph{extent}, which
#' characterises the minimum and maximum value of the vertex coordinates and
#' which is thus derived "on the fly" from the vertices. Instead it has a
#' \emph{reference window}, which is sort of a second extent that may be bigger
#' than \code{extent} and which determines the relative position of the vertices
#' when plotting.
#' @section Methods: So far the following methods have been defined: \itemize{
#'   \item Getters: \cr \code{length}, \code{subset}, \code{show},
#'   \code{\link{getWindow}}, \code{\link{getExtent}}, \code{\link{getCRS}},
#'   \code{\link{getTable}}, \code{\link{getSubset}}, \code{\link{getHistory}},
#'   \item Setters: \cr \code{\link{setCRS}} }
#' @slot type [\code{character(1)}]\cr the type of feature, recently either
#'   \code{"point"}, \code{"line"} or \code{"polygon"}.
#' @slot coords [\code{data.frame(1)}]\cr the \code{id}, \code{x} and \code{y}
#'   coordinates per vertex.
#' @slot attr [\code{data.frame(1)}]\cr arbitrary number of attributes per
#'   unique id in \code{coords}
#' @slot window [\code{data.frame(1)}]\cr the minimum and maximum value in x and
#'   y dimension of the reference window in which the \code{geom} dwells.
#' @slot scale [\code{character(1)}]\cr whether the vertex coordinates are
#'   stored as \code{"absolute"} values, or \code{"relative"} to \code{window}.
#' @slot crs [\code{character(1)}]\cr the coordinate reference system in proj4
#'   notation.
#' @slot history [\code{list(.)}]\cr a list of steps taken to derive the
#'   \code{geom} in focus.

geom <- setClass(Class = "geom",
                 slots = c(type = "character",
                           coords = "data.frame",
                           attr = "data.frame",
                           window = "data.frame",
                           scale = "character",
                           crs = "character",
                           history = "list"
                 )
)

# setValidity("geom", function(){

# this must include a warning when for instance a polygon has less than 3 vertices, other such cases?
#
#   if(existsGeom){
#     assertNames(names(geom), identical.to = c("coords", "extent", "type"))
#     assertNames(names(geom$coords), permutation.of = c("x", "y", "id"))
#     assertNames(names(geom$extent), permutation.of = c("x", "y"))
#     assertSubset(geom$type, choices = c("point", "points", "line", "lines", "polygon", "polygons"))
#   }
# })

# setClass("SpatialPolygonsDataFrame",
#          contains = "SpatialPolygons",
#          slots = c(data = "data.frame"),
#          validity = function(object) {
#            if (!inherits(object@data, "data.frame"))
#              stop("data should be of class data.frame")
#            if (nrow(object@data) != length(object@polygons))
#              stop("number of rows in data.frame and polygons in SpatialPolygons don't match")
#            return(TRUE)
#          }
# )

#' Theme class and methods
#' @slot title [\code{list(3)}]\cr
#' @slot box [\code{list(4)}]\cr
#' @slot xAxis [\code{list(5)}]\cr
#' @slot yAxis [\code{list(5)}]\cr
#' @slot grid [\code{list(5)}]\cr
#' @slot legend [\code{list(10)}]\cr
#' @slot geom [\code{list(7)}]\cr
#' @slot raster [\code{list(2)}]\cr

themeClass <- setClass(Class = "rtTheme",
                    slots = c(title = "list",
                              box = "list",
                              xAxis = "list",
                              yAxis = "list",
                              grid = "list",
                              legend = "list",
                              geom = "list",
                              raster = "list"
                    )
)

