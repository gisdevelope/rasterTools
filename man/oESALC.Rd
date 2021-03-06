% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oESALC.R
\name{oESALC}
\alias{oESALC}
\alias{downloadESALC}
\title{Obtain global ESA CCI land-cover data}
\usage{
oESALC(mask = NULL, years = NULL, assertQuality = TRUE, ...)

downloadESALC(file = NULL, localPath = NULL)
}
\arguments{
\item{mask}{[\code{geom} | \code{Spatial*} | \code{sf}]\cr spatial object of
which the extent is the area of interest.}

\item{years}{[\code{integerish(.)}]\cr year(s) for which ESA CCI land-cover
data should be extracted; see Details.}

\item{assertQuality}{[\code{logical(1)}] (not supported yet)\cr should the
quality flags of the ESA CCI land-cover dataset be extracted (\code{TRUE},
default) or should merely the data-layer be extracted (\code{FALSE})?}

\item{...}{[various]\cr other arguments.}

\item{file}{[\code{character(1)}]\cr the name of the file to download.}

\item{localPath}{[\code{character(1)}]\cr the local path where files are
supposed to be stored (i.e. on your harddisc).}
}
\description{
Obtain data from the 'ESA CCI land-cover'
\href{http://maps.elie.ucl.ac.be/CCI/viewer/index.php}{dataset}
(\href{http://maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf}{User
Guide}).
}
\details{
The object provided in \code{mask} is treated as a single mask,
  irrespective of that object consisting of only one or several features. The
  extent comprising all features (point(s), line(s), polygon(s)) is used as
  area of interest. This is in contrast to \code{\link{obtain}}, where a mask
  may consist of several features, each of which are treated as seperate
  mask.
}
\section{Functions}{
\itemize{
\item \code{downloadESALC}: function to download ESA CCI land-cover data
}}

\examples{
\dontrun{

myESALC <- oESALC(mask = rtGeoms$mask, years = 2005)
visualise(raster = myESALC, trace = TRUE)

# get the (updated) bibliography
reference(style = "bibtex")
}
}
\seealso{
Other obtain operators (Global): \code{\link{oGFC}},
  \code{\link{oMODIS}}, \code{\link{oWCLIM}}
}
\concept{obtain operators (Global)}
