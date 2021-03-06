% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.operators.R
\name{rMask}
\alias{rMask}
\title{Select cells of a raster based on a mask}
\usage{
rMask(obj, mask = NULL, background = NULL)
}
\arguments{
\item{obj}{[\code{RasterLayer(1)}]\cr The object to modify.}

\item{mask}{[\code{RasterLayer(1)} | \code{matrix(1)} | \code{geom(1)}]\cr
Either binary object of the same dimension as \code{obj} where the cells
that should be retained have the value 1 and all other cells the value 0 or\cr
\code{geom} where values inside the geom should be retained.}

\item{background}{[\code{integerish(1)}]\cr the value any cell with value NA
should have.}
}
\value{
A \code{RasterLayer} of the same dimensions as \code{obj}, in which
  all cells with value 0 in the mask have been set to \code{NA} and all other
  values are retained.
}
\description{
Select cells of a raster based on a mask
}
\details{
If used in an algorithm, \code{mask} can also contain the name of a
  sub-algorithm to use the final output thereof as mask. Moreover, \code{mask
  = "input"} would select the original raster as mask.
}
\examples{
input <- rtData$continuous
m <- matrix(nrow = 56, ncol = 60, data = 0)
m[c(5:25), c(5:50)] <- 1

visualise(rMask(input, mask = m))

\dontrun{

# determine mask interactively
mask <- geomPolygon(template = input, vertices = 5, show = T, col = "deeppink")
mask <- gToRaster(mask)

visualise(raster = rMask(obj = input, mask = mask))
}
}
\seealso{
Other operators to select a subset of cells: \code{\link{rBounded}},
  \code{\link{rGreater}}, \code{\link{rLess}},
  \code{\link{rMatch}}
}
\concept{operators to select a subset of cells}
