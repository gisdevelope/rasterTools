% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadData.R
\name{loadData}
\alias{loadData}
\title{Import spatial datasets}
\usage{
loadData(files = NULL, layer = NULL, dataset = NULL,
  localPath = NULL, verbose = FALSE, ...)
}
\arguments{
\item{files}{[\code{character(.)} | \code{data.frame(1)}]\cr the files, as
they are called in \code{localPath}. \code{files} also accepts the
(subsetted) output of \code{\link{catalog}}, i.e. a \code{data.frame} with
the two columns \code{original} and \code{abbr}.}

\item{layer}{[\code{character(.)} | \code{integerish(.)}]\cr vetor of the
same length as \code{files} with name or position of the layer that shall
be loaded.}

\item{dataset}{[\code{character(1)}]\cr character vector of the dataset for
which \code{files} should be imported (see \code{\link{obtain}} for all
datasets); in case this is given, \code{localPath} is taken from the
internal default for the respective dataset (see
\code{\link{setPaths}}).}

\item{localPath}{[\code{character(1)}]\cr the local path from where files
shall be loaded. If the directory does not exist, it is created and the
missing data are downloaded, given .}

\item{verbose}{[\code{logical(1)}]\cr should additional information be
printed (\code{TRUE}) or should it be suppressed (\code{FALSE}, default)?}

\item{...}{[various]\cr other arguments of the load operators.}
}
\value{
the file to load. If there is more than one file specified, a list of
  those files.
}
\description{
Meta function to import, i.e. load or download and unpack spatial datasets
and other files containing spatial data.
}
\details{
\code{importData} checks whether the required files are available in
  \code{localPath} and if this is not given, in the working directory. If
  nothing is found there but a dataset is given, it attempts to download the
  files from the respective online path (see \code{rtPaths}). Please take a
  look at it and in case an online resource has been altered, you can adapt
  it here (see \code{\link{setPaths}}).
}
\examples{
require(magrittr)

myLocations <- loadData(files = "locations.csv",
                   localPath = system.file("test_datasets", package="rasterTools")) \%>\%
  geomRectangle() \%>\%
  setCRS(crs = projs$laea)
}
\seealso{
The specific load operators: \code{\link{load_kml}},
  \code{\link{load_csv}}, \code{\link{load_hdf}}, \code{load_tif},
  \code{load_dbf}, \code{load_shp}, \code{\link{load_svg}}
}
