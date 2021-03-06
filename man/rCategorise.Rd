% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.operators.R
\name{rCategorise}
\alias{rCategorise}
\title{Assign categories to the values in a raster}
\usage{
rCategorise(obj, breaks = NULL, n = NULL)
}
\arguments{
\item{obj}{[\code{RasterLayer(1)}]\cr The object to modify.}

\item{breaks}{[\code{integerish(.)}]\cr the values, where categories should
be delimited.}

\item{n}{[\code{integerish(1)}]\cr number of categories.}
}
\value{
A \code{RasterLayer} of the same dimension as \code{obj}, in which
  the cells have the category number into which their values fall.
}
\description{
Assign categories to the values in a raster
}
\details{
Using \code{n} will determine \code{breaks} based on the value-range
  of \code{obj} so that the values are assigned to n categories.

  Assigning \code{breaks} is mostly usefull when values are to be non-linear,
  such as \code{log(min:max)*max/log(max)}, but could also be \code{seq(min,
  max, length.out = 21)}, which corresponds to \code{n = 20}.
}
\examples{
input <- rtData$continuous
visualise(rCategorise(input, n = 5))

# use as algorithm in modify, to combine two iterations in one run
algorithm <- list(list(operator = "rCategorise", breaks = c(25, 50, 75, 90)),
                  list(operator = "rCategorise", breaks = log(1:5)*5/log(5)*20))

obj_mod <- modify(input, by = algorithm, merge = TRUE)
visualise(obj_mod)
}
\seealso{
Other operators to modify cell values: \code{\link{rBinarise}},
  \code{\link{rDistance}}, \code{\link{rFillNA}},
  \code{\link{rOffset}}, \code{\link{rPermute}},
  \code{\link{rRange}}, \code{\link{rSubstitute}}
}
\concept{operators to modify cell values}
