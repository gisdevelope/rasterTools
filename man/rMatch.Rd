% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.operators.R
\name{rMatch}
\alias{rMatch}
\title{Match cells of a raster with a kernel}
\usage{
rMatch(obj, kernel = NULL, rotate = TRUE, background = NULL)
}
\arguments{
\item{obj}{[\code{RasterLayer(1)}]\cr The object to modify.}

\item{kernel}{[\code{matrix(.)} | \code{list(.)} thereof]\cr scan the
raster with this kernel (default is a 3 by 3 cells diamond kernel).}

\item{rotate}{[\code{logical(1)}]\cr should the kernel be applied for all
possible rotations (\code{TRUE}, default) or should the kernel be used as
is (\code{FALSE})?}

\item{background}{[\code{integerish(1)}]\cr the value any cell with value NA
should have.}
}
\value{
A \code{RasterLayer} or \code{RasterStack} of the same dimension as
  \code{obj} in which all cells that match with the kernel(s) have the kernel
  value and all other cells have the value \code{background}.
}
\description{
Match cells of a raster with a kernel
}
\details{
The cells of a kernel can have the values 0, 1 and NA. The kernel
  cells with values 0 and 1 are matched accurately in the input raster. The
  kernel cells with value NA will be ignored.

This is also known as the 'hit-or-miss'-transform.\cr\cr Wrapper of
  \code{\link{rMorph}} with blend = 2 and merge = 12.
}
\examples{
input <- rtData$continuous
binarised <- rBinarise(input, thresh = 30)

# match isolated cells
(kIso <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3))
matched <- rMatch(binarised, kernel = kIso)
visualise(matched)

# match all right angled corners
(kCorner <- matrix(c(NA, 1, NA, 0, 1, 1, 0, 0, NA), 3, 3))
matched <- rMatch(binarised, kernel = kCorner, background = 0)
visualise(matched, new = TRUE)

# match north-east facing right angled corners and isolated cells
matched <- rMatch(binarised, kernel = list(kIso, kCorner), rotate = FALSE)
visualise(matched, new = TRUE)

# match endpoints of a skeleton
skeletonised <- rSkeletonise(binarised, background = 0)
(kEnd <- matrix(c(NA, 0, 0, NA, 1, 0, NA, 0, 0), 3, 3))
endpoints <- rMatch(skeletonised, kernel = kEnd, background = 0)

# match triple points (conjunctions) of a skeleton
kConj <- list(matrix(c(NA, 0, 1, 1, 1, NA, NA, 0, 1), 3, 3),
              matrix(c(1, NA, 1, NA, 1, NA, NA, NA, 1), 3, 3),
              matrix(c(NA, 1, NA, 0, 1, 1, 1, 0, NA), 3, 3))
conjunctions <- rMatch(skeletonised, kernel = kConj, background = 0)
out <- raster::stack(
  rBlend(skeletonised, overlay = endpoints),
  rBlend(skeletonised, overlay = conjunctions[[1]]),
  rBlend(skeletonised, overlay = conjunctions[[2]]),
  rBlend(skeletonised, overlay = conjunctions[[3]]))
names(out) <- c("endpoints", "conj1", "conj2", "conj3")
visualise(out)
}
\seealso{
Other operators to select a subset of cells: \code{\link{rBounded}},
  \code{\link{rGreater}}, \code{\link{rLess}},
  \code{\link{rMask}}
}
\concept{operators to select a subset of cells}
