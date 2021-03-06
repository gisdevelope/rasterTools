% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geometries.R
\name{geomPolygon}
\alias{geomPolygon}
\alias{geomTriangle}
\alias{geomSquare}
\alias{geomRectangle}
\alias{geomHexagon}
\title{Create a polygon geometry}
\usage{
geomPolygon(anchor = NULL, window = NULL, template = NULL,
  features = 1, vertices = NULL, regular = FALSE, show = FALSE,
  ...)

geomTriangle(anchor = NULL, window = NULL, template = NULL,
  features = 1, show = FALSE, ...)

geomSquare(anchor = NULL, window = NULL, template = NULL,
  features = 1, show = FALSE, ...)

geomRectangle(anchor = NULL, window = NULL, template = NULL,
  features = 1, show = FALSE, ...)

geomHexagon(anchor = NULL, window = NULL, template = NULL,
  features = 1, show = FALSE, ...)
}
\arguments{
\item{anchor}{[\code{geom} | \code{data.frame(1)}]\cr Object to derive the
\code{geom} from. In case of \code{data.frame}, it must include column
names \code{x}, \code{y} and optinal variables such as \code{id}; see
Examples.}

\item{window}{[\code{data.frame(1)}]\cr in case the reference window deviates
from the bounding box of \code{anchor} (minimum and maximum values),
specify this here.}

\item{template}{[\code{RasterLayer(1)} | \code{matrix(1)}]\cr Gridded object
that serves as template to sketch the geometry.}

\item{features}{[\code{integerish(1)}]\cr number of geometries to create.}

\item{vertices}{[\code{integerish(.)}]\cr number of vertices per geometry;
will be recycled if it does not have as many elements as specified in
\code{features}.}

\item{regular}{[\code{logical(1)}]\cr should the polygon be regular, i.e.
point symmetric (\code{TRUE}) or should the vertices be selected according
to \code{anchor} or \code{vertices} (\code{FALSE}, default)?}

\item{show}{[\code{logical(1)}]\cr should the geometry be plotted
(\code{TRUE}) or should it not be plotted (\code{FALSE}, default)? In case
\code{template} is set, it is automatically \code{TRUE}.}

\item{...}{[various]\cr graphical parameter, in case \code{show = TRUE}; see
\code{\link{gpar}}.}
}
\value{
An invisible \code{geom}.
}
\description{
Create any (regular) polygon geometry (of class \code{\link{geom}}) either by
specifying its parameters or by sketching it.
}
\details{
The arguments \code{anchor} and \code{template} have \code{NULL}
  value, because leaving them unset is meant to result in a specific
  behaviour: \itemize{ \item \code{anchor}: if unset, this argument triggers
  that the geometry is created interactively (hence, \code{template} must be
  set); if set, the input provided is used to parameterise the geometry:
  \itemize{ \item if \code{regular = FALSE} the resulting geometry is the
  line connecting the vertices, \item if \code{regular = TRUE}, only the
  first two coordinates are considered as center and indicating the (outer)
  radius.} \item \code{template}: if unset, this argument triggers that the
  geometry is created programmatically (hence, \code{anchor} must be set).}
}
\section{Functions}{
\itemize{
\item \code{geomTriangle}: wrapper of geomPolygon where \code{vertices = 3} and
\code{regular = TRUE}.

\item \code{geomSquare}: wrapper of geomPolygon where \code{vertices = 4},
\code{regular = TRUE} and a rotation by 45° about the centroid has been
applied.

\item \code{geomRectangle}: wrapper of geomPolygon where \code{vertices = 2},
\code{regular = FALSE} and the two complementing corners are derived from
the two given opposing corners.

\item \code{geomHexagon}: wrapper of geomPolygon where \code{vertices = 6} and
\code{regular = TRUE}.
}}

\examples{
# create a polygon programmatically
coords <- data.frame(x = c(40, 70, 70, 50),
                     y = c(40, 40, 60, 70))
window <- data.frame(x = c(0, 80),
                     y = c(0, 80))
                     
# if no window is set, the bounding box (i.e. min/max values) will be set as window
(aGeom <- geomPolygon(anchor = coords))

# the vertices are plottet relative to the window
aTriangle <- geomPolygon(anchor = coords, window = window, vertices = 3, regular = TRUE, 
                         fill = "darkorange", show = TRUE)
(geomHexagon(anchor = coords, col = "green", show = TRUE))

# if a geom is used in 'anchor', its properties (e.g. 'window') are passed on
grid::grid.newpage()
aGeom <- geomPolygon(anchor = coords, window = window, fill = "deeppink", show = TRUE)
anExtent <- geomRectangle(anchor = aGeom, show = TRUE)

# geoms with more than one element are treated element-wise
aGeom <- gGroup(geom = aGeom, index = c(1, 2, 1, 2))
visualise(geom = aGeom)
itsExtent <- geomRectangle(anchor = aGeom, show = TRUE)

\dontrun{

input <- rtData$continuous

# create a square interactively
squareGeom <- geomSquare(template = input, show = TRUE, col = "orange")

# ... or an approximate circle (actually a hectogon)
circleGeom <- geomPolygon(template = input, vertices = 100, regular = TRUE,
                          show = TRUE, col = "deeppink")

# create two arbitrary polygons interactively
polyGeom <- geomPolygon(template = input, features = 2, vertices = c(4, 6),
                        col = "green", lwd = 1, lty = "dashed", show = TRUE)
}
}
\seealso{
Tools to modify geometries: \code{\link{gRotate}},
  \code{\link{gScale}}, \code{\link{gGroup}}\cr Tools to transform geometries
  to other classes: \code{\link{gToSp}},  \code{gToSf},
  \code{\link{gToRaster}}, \code{\link{gFrom}}

Other shapes: \code{\link{geomPoint}},
  \code{\link{geomRand}}
}
\concept{shapes}
