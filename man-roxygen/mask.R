#' @param mask [\code{geom} | \code{Spatial*} | \code{sf}]\cr spatial object of
#'   which the extent is the area of interest.
#' @details The object provided in \code{mask} is treated as a single mask,
#'   irrespective of that object consisting of only one or several features. The
#'   extent comprising all features (point(s), line(s), polygon(s)) is used as
#'   area of interest. This is in contrast to \code{\link{obtain}}, where a mask
#'   may consist of several features, each of which are treated as seperate
#'   mask.
