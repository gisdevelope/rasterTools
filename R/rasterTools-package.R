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
#' \code{\link{obtain}} spatial (gridded) datasets from the Landsat or Sentinel
#' Missions, such as MODIS, GFC and many other ready to use datasets. Moreover,
#' you can \code{\link{generate}} spatial models, including neutral landscape
#' models. Once you have your spatial data in place, you can
#' \code{\link{modify}} them, which may be required to identify objects about
#' which you want to inquire. You would then \code{\link{measure}} the
#' potentially modified data with the help of a modular system that allows you
#' to use nearly all landscape metrics or even develop your own.
#'
#' Various helper functions that create geometries and tools to work with these
#' are included. Geometries (such as \code{\link{geomPoint}}, \code{geomCurve}
#' or \code{\link{geomPolygon}}) can be created programmatically or
#' interactively, can be modified (e.g. \code{\link{gRotate}}) and transformed
#' into sp, sf or raster objects. The transformed objects can be fed into other
#' workflows or might serve a purpose within \code{rasterTools}. For instance,
#' \code{\link{gToRaster}} is an important tool to create raster masks, binary
#' objects where everything covered by 1 is to be selected/processed and
#' everything covered by 0 is not.
#'
#' \code{rasterTools} comes with a function to \code{\link{visualise}} gridded
#' objects and geometries. Gridded objects can be either a \code{Raster*} or a
#' \code{matrix} and hence everything that can be coerced to a matrix. Gridded
#' objects with more than one layer are automatically presented in panels.
#'
#' \code{rasterTools} has been conceptualised so that the core functions are
#' modular. The core functions manage all the code-logic that is common for the
#' task at hand. Code that manages, for instance, "loading the MODIS dataset" is
#' outsourced to a separate function (the \code{\link{oMODIS}} operator). New
#' operators to all the core functions and to \code{\link{loadData}} can hence
#' be devised easily. If you want to contribute, please study the
#' \href{https://ehrmanns.github.io/rastertools/articles/contribute.html}{Contribution
#' Guidelines} or create an issue on
#' \href{https://github.com/EhrmannS/rastertools/issues}{github}.
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
#' # load a mask from some file,
#' # myMask <- loadData(files = "myLocations2.csv",
#' #                    localPath = rtPaths$project) %>%
#' #    gGroup(distance = 10000) %>%
#' #    geomPolygon() %>%
#' #    gToSp(crs = LAEA)
#'
#' # grab the data
#' myData <- obtain(data = myDatasets, mask = myMask)
#' }
#' @docType package
#' @name rasterTools
NULL