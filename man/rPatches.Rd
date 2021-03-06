% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.operators.R
\name{rPatches}
\alias{rPatches}
\title{Determine foreground patches in a raster}
\usage{
rPatches(obj, kernel = NULL, background = NULL)
}
\arguments{
\item{obj}{[\code{RasterLayer(1)}]\cr The object to modify.}

\item{kernel}{[\code{matrix(1)}]\cr scan the raster with this kernel (default
is a 3 by 3 cells diamond kernel).}

\item{background}{[\code{integerish(1)}]\cr the value any cell with value NA
should have.}
}
\value{
A \code{RasterLayer} of the same dimension as \code{obj}, in which
  neighbouring cells of the foreground have been assigned the same value,
  forming patches.
}
\description{
(Foreground) Patches are sets of cells which are connected either directly or
via other cells in a binarised raster and which should hence be treated as
distinct objects.
}
\examples{
input <- rtData$continuous
binarised <- rBinarise(input, thresh = 30)
visualise(rPatches(binarised))
}
\seealso{
Other operators to determine objects: \code{\link{rCentroid}},
  \code{\link{rSkeletonise}}
}
\concept{operators to determine objects}
