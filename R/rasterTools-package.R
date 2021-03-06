#' rasterTools: obtain and process earth observation data
#'
#' The \code{rasterTools} package is based on the four core-functions
#' \code{\link{obtain}}, \code{\link{generate}}, \code{\link{modify}} and
#' \code{\link{measure}}.
#'
#' In each of the core functions you provide an \emph{algorithm} based on which
#' the specific task at hand shall be carried out. An algorithm is a list of
#' scope specific functions and their arguments (\emph{operators}) that carry
#' out a specific (spatial) computation (see Examples below). These algorithms
#' are an easy to share and transparent "recipe" of a certain (sequence of)
#' spatial operation.
#'
#' You might find \code{rasterTools} useful when you want to
#' \code{\link{obtain}} spatial (gridded) datasets or \code{\link{generate}}
#' spatial models, including neutral landscape models. Once you have your
#' spatial data in place, you can \code{\link{modify}} them, which may be
#' required to identify objects about which you want to inquire. You would then
#' \code{\link{measure}} the potentially modified data with the help of a
#' modular system that allows you to use nearly all landscape metrics or even
#' develop your own.
#'
#' Various helper functions that create geometries and tools to work with these
#' are included. Geometries (see \code{\link{geomPoint}}, \code{geomCurve} or
#' \code{\link{geomPolygon}}) can be derived from other spatial objects (see
#' \code{\link{gFrom}} or be created programmatically or interactively. They can
#' be modified (e.g. \code{\link{gRotate}}) and transformed into sp, sf or
#' raster objects.
#'
#' \code{rasterTools} comes with a function to \code{\link{visualise}} gridded
#' objects and geometries. Gridded objects can be either a \code{Raster*} or a
#' \code{matrix} and hence everything that can be coerced to a matrix. Gridded
#' objects with more than one layer are automatically presented in panels.
#'
#' \code{rasterTools} has been conceptualised so that the core functions are
#' modular. The core functions manage all the code-logic that is common for the
#' class of tasks, such as "loading spatial datasets". Code that manages a
#' particular task of that class, such as "loading the MODIS dataset", is
#' outsourced to a separate function (the \code{\link{oMODIS}} operator). New
#' operators to all the core functions can hence be devised easily. If you want
#' to contribute, please study the
#' \href{https://ehrmanns.github.io/rasterTools/articles/contribute.html}{Contribution
#' Guidelines} or create an issue on
#' \href{https://github.com/EhrmannS/rasterTools/issues}{github}.
#' @examples
#' \dontrun{
#'
#' require(magrittr)
#'
#' # define an algorithm
#' myDatasets <- list(list(operator = "oGFC", years = 2006)),
#'                    list(operator = "oCLC", years = 2006),
#'                    list(operator = "oMODIS", product = "mod17a3",
#'                         period = 2006, layer = 2))
#'
#' load a mask from some file,
#' myMask <- loadData(files = "locations.csv",
#'                    localPath = system.file("csv", package="rasterTools")) %>%
#'   gGroup(distance = 10000) %>%
#'   geomRectangle() %>%
#'   gToSp(crs = projs$laea)
#'
#' # grab the data
#' myData <- obtain(data = myDatasets, mask = myMask)
#' }
#' @docType package
#' @name rasterTools
NULL