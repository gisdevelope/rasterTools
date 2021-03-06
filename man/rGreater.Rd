% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.operators.R
\name{rGreater}
\alias{rGreater}
\title{Select cells with values above a threshold in a raster}
\usage{
rGreater(obj, thresh, background = NULL)
}
\arguments{
\item{obj}{[\code{RasterLayer(1)}]\cr The object to modify.}

\item{thresh}{[\code{numeric(1)}]\cr values above this threshold should be
retained.}

\item{background}{[\code{integerish(1)}]\cr the value any cell with value NA
should have.}
}
\value{
A \code{RasterLayer} of the same dimensions as \code{obj}, in which
  all values lower than \code{thresh} have been set to \code{background}.
}
\description{
Select cells with values above a threshold in a raster
}
\examples{
input <- rtData$continuous
visualise(rGreater(input, thresh = 40))
}
\seealso{
Other operators to select a subset of cells: \code{\link{rBounded}},
  \code{\link{rLess}}, \code{\link{rMask}},
  \code{\link{rMatch}}
}
\concept{operators to select a subset of cells}
