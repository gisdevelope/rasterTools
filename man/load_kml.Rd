% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadData.R
\name{load_kml}
\alias{load_kml}
\title{Load \code{kml} files}
\usage{
load_kml(path, layer = NULL)
}
\arguments{
\item{path}{[\code{character(1)}]\cr the local path of the file to load.}

\item{layer}{[\code{character(1)}]\cr the layer name.}
}
\value{
a geom of the contents of the kml file.
}
\description{
kml is the short form of \emph{Keyhole Markup Language}, which uses the
\code{XML} format.
}
\details{
This is a helper to \code{\link{loadData}} and is not intended to be used on
its own.
}
\seealso{
Other loaders: \code{\link{load_csv}},
  \code{\link{load_hdf}}, \code{\link{load_svg}}
}
\concept{loaders}
