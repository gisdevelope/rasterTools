#' Create a point geometry
#'
#' @param anchor [\code{data.frame(1)}]\cr Object to derive the \code{geom}
#'   from. It must include column names \code{x}, \code{y} and optinal variables
#'   such as \code{fid}; see Examples.
#' @template window
#' @template template
#' @param vertices [\code{integer(1)}]\cr number of vertices.
#' @param show [\code{logical(1)}]\cr in case \code{template} is set, should the
#'   points be plotted (\code{TRUE}) or should it not be plotted (\code{FALSE},
#'   default)?
#' @param ... [various]\cr graphical parameter, in case \code{show = TRUE}; see
#'   \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @family shapes
#' @seealso Tools to modify geometries: \code{\link{gRotate}},
#'   \code{\link{gScale}}, \code{\link{gGroup}}\cr Tools to transform geometries
#'   to other classes: \code{\link{gToSp}},  \code{gToSf},
#'   \code{\link{gToRaster}}, \code{\link{gFrom}}
#' @examples
#' # create points programmatically
#' somePoints <- data.frame(X = c(5190599, 5222810, 5041066, 5234735,
#'                          5326537, 5027609, 5281527, 5189955), Y = c(3977612,
#'                          4060164, 3997230, 4117856, 4028167, 3971119, 4118207,
#'                          4062838))
#' (pointsGeom <- geomPoint(anchor = somePoints, col = "darkorange", show = TRUE))
#'
#' \dontrun{
#'
#' input <- rtData$continuous
#'
#' # create points interactively
#' myPoints <- geomPoint(template = input, vertices = 5, show = TRUE, col = "deeppink")
#' myPoints <- gGroup(myPoints, index = rep(1, 5))
#' anExtent <- geomRectangle(myPoints, show = TRUE, col = "green")
#' }
#' @importFrom checkmate testDataFrame assertNames testNull assert testClass
#'   assertLogical assertIntegerish
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom methods new
#' @export

geomPoint <- function(anchor = NULL, window = NULL, template = NULL,
                      vertices = NULL, show = FALSE, ...){

  # check arguments
  anchorExists <- !testNull(anchor)
  if(anchorExists){
    assertDataFrame(anchor, types = "numeric", any.missing = FALSE, min.cols = 2)
    colnames(anchor) <- tolower(colnames(anchor))
    assertNames(names(anchor), must.include = c("x", "y"), subset.of = c("fid", "vid", "x", "y"))
  }
  windowExists <- !testNull(window)
  if(windowExists){
    assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
    colnames(window) <- tolower(colnames(window))
    assertNames(names(window), must.include = c("x", "y"))
  } else{
    if(anchorExists){
      window <- tibble(x = c(min(anchor$x), max(anchor$x)),
                       y = c(min(anchor$y), max(anchor$y)))
    }
  }
  templateExists <- !testNull(template)
  if(templateExists){
    assert(
      testClass(template, "RasterLayer"),
      testClass(template, "matrix")
    )
  }
  if(!anchorExists & !templateExists){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertLogical(show)
  if(!anchorExists){
    assertIntegerish(vertices, min.len = 1, any.missing = FALSE)
  } else{
    assertIntegerish(vertices, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  }

  # get some raster properties
  if(templateExists){
    if(testClass(template, "RasterLayer")){
      tempName <- names(template)
      dims <- dim(template)
      projection <- crs(template, asText = TRUE)
    } else{
      tempName <- "layer"
      dims <- dim(template)
      projection <- NA
    }
  } else{
    tempName <- "layer"
    projection <- NA
  }

  # if anchor does not exists, make it
  if(!anchorExists){
    message("please click the ", vertices, " vertices.")
    coords <- locate(raster = template, samples = vertices, panel = tempName, silent = TRUE, show = FALSE, ...)
    window <- tibble(x = c(0, dims[2]),
                     y = c(0, dims[1]))
    anchor <- tibble(x = coords$x,
                     y = coords$y)
  } else{
    if(!windowExists){
      window <- tibble(x = c(min(anchor$x), max(anchor$x)),
                       y = c(min(anchor$y), max(anchor$y)))
    }
  }

  if(!"vid" %in% names(anchor)){
    anchor <- bind_cols(vid = seq_along(anchor$x), anchor)
  }
  if(!"fid" %in% names(anchor)){
    anchor <- bind_cols(fid = seq_along(anchor$x), anchor)
  }
  anchor <- anchor[c("fid", "vid", "x", "y")]
  theGeom <- new(Class = "geom",
                 type = "point",
                 coords = anchor,
                 attr = tibble(fid = unique(anchor$fid), n = as.data.frame(table(anchor$fid))$Freq),
                 window = tibble(x = rep(window$x, each = 2), y = c(window$y, rev(window$y))),
                 scale = "absolute",
                 crs = as.character(projection),
                 history = list(paste0("geometry was created as 'point'")))

  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = theGeom, new = TRUE, ...)
    } else{
      visualise(geom = theGeom, ...)
    }
  }

  invisible(theGeom)
}

# Sketch a curve
#
# This creates the coordinates to any continuous curve.
# @template anchor
# @template template
# @param control .
# @param weights .
# @param closed .
# @param show [\code{logical(1)}]\cr in case \code{template} is set, should the
#   geometry be plotted (\code{TRUE}, default) or should it not be plotted
#   (\code{FALSE})?
# @param ... [various]\cr
#   graphical parameter, in case \code{show = TRUE}.
# @details m
# @return An invisible geometry object.
# @family shapes

# geomCurve <- function(anchor = NULL, template = NULL, control, weights = 1, closed = FALSE,
#                       show = TRUE, ...){
#
#   # http://www.antigrain.com/research/bezier_interpolation/
#   # https://en.wikipedia.org/wiki/B%C3%A9zier_curve
#
#   if(missing(template)) stop("please provide a template.")
#   # check that center is vector with length 2
#   # check that radius does not exceed the dim of template
#   if(is.null(names(template))){
#     tempName <- "layer"
#   } else{
#     tempName <- names(template)
#   }
#
#   #   # circleGrob()
#   #
#   #   if(show){
#   #     # raster::plot(template)
#   #   }
#   #
#   #   message("please click first in the center of the ellipse and then on a location through which the perimeter should go.\n")
#   #
#   #   # coords <- locator(n = 2)
#   #   # coords <- cbind(coords$x, coords$y)
#   #
#   #   width <- dist(coords)*2
#   #   height <- width*ratio
#   #
#
#   #
#   #   mask <- SpatialPolygons(list(Polygons(list(Polygon(ellipse)), 1)))
#   # plot(mask, add = T)
#   #   if(show){
#   #     raster::plot(mask, add = TRUE, ...)
#   #   }
#   #   invisible(mask)
#   #
#   #   # this is from spSketch
#   #   # center <- raster::click(obj, n = 1, cell = TRUE, show = FALSE)$cell
#   #   # center <- raster::rasterToPoints(obj, spatial = TRUE)[center,]
#   #   # outer <- unlist(locator(n = 1))
#   #   # circle <- rbind(center@coords, outer)
#   #   # mask <- rgeos::gBuffer(spgeom = center, width = dist(circle))
#
# }
# geomLine <- function(){
#   geomCurve()
# }
# geomCircle <- function(){
#   geomCurve()
# }
# geomEllipse <- function(){
#   geomCurve()
# }

#' Create a polygon geometry
#'
#' Create any (regular) polygon geometry (of class \code{\link{geom}}) either by
#' specifying its parameters or by sketching it.
#' @template anchor
#' @template window
#' @template template
#' @template features
#' @param vertices [\code{integerish(.)}]\cr number of vertices per geometry;
#'   will be recycled if it does not have as many elements as specified in
#'   \code{features}.
#' @param regular [\code{logical(1)}]\cr should the polygon be regular, i.e.
#'   point symmetric (\code{TRUE}) or should the vertices be selected according
#'   to \code{anchor} or \code{vertices} (\code{FALSE}, default)?
#' @param show [\code{logical(1)}]\cr should the geometry be plotted
#'   (\code{TRUE}) or should it not be plotted (\code{FALSE}, default)? In case
#'   \code{template} is set, it is automatically \code{TRUE}.
#' @param ... [various]\cr graphical parameter, in case \code{show = TRUE}; see
#'   \code{\link{gpar}}.
#' @return An invisible \code{geom}.
#' @details The arguments \code{anchor} and \code{template} have \code{NULL}
#'   value, because leaving them unset is meant to result in a specific
#'   behaviour: \itemize{ \item \code{anchor}: if unset, this argument triggers
#'   that the geometry is created interactively (hence, \code{template} must be
#'   set); if set, the input provided is used to parameterise the geometry:
#'   \itemize{ \item if \code{regular = FALSE} the resulting geometry is the
#'   line connecting the vertices, \item if \code{regular = TRUE}, only the
#'   first two coordinates are considered as center and indicating the (outer)
#'   radius.} \item \code{template}: if unset, this argument triggers that the
#'   geometry is created programmatically (hence, \code{anchor} must be set).}
#' @family shapes
#' @seealso Tools to modify geometries: \code{\link{gRotate}},
#'   \code{\link{gScale}}, \code{\link{gGroup}}\cr Tools to transform geometries
#'   to other classes: \code{\link{gToSp}},  \code{gToSf},
#'   \code{\link{gToRaster}}, \code{\link{gFrom}}
#' @examples
#' # create a polygon programmatically
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#'                      
#' # if no window is set, the bounding box (i.e. min/max values) will be set as window
#' (aGeom <- geomPolygon(anchor = coords))
#'
#' # the vertices are plottet relative to the window
#' aTriangle <- geomPolygon(anchor = coords, window = window, vertices = 3, regular = TRUE, 
#'                          fill = "darkorange", show = TRUE)
#' (geomHexagon(anchor = coords, col = "green", show = TRUE))
#'
#' # if a geom is used in 'anchor', its properties (e.g. 'window') are passed on
#' grid::grid.newpage()
#' aGeom <- geomPolygon(anchor = coords, window = window, fill = "deeppink", show = TRUE)
#' anExtent <- geomRectangle(anchor = aGeom, show = TRUE)
#'
#' # geoms with more than one element are treated element-wise
#' aGeom <- gGroup(geom = aGeom, index = c(1, 2, 1, 2))
#' visualise(geom = aGeom)
#' itsExtent <- geomRectangle(anchor = aGeom, show = TRUE)
#'
#' \dontrun{
#'
#' input <- rtData$continuous
#'
#' # create a square interactively
#' squareGeom <- geomSquare(template = input, show = TRUE, col = "orange")
#'
#' # ... or an approximate circle (actually a hectogon)
#' circleGeom <- geomPolygon(template = input, vertices = 100, regular = TRUE,
#'                           show = TRUE, col = "deeppink")
#'
#' # create two arbitrary polygons interactively
#' polyGeom <- geomPolygon(template = input, features = 2, vertices = c(4, 6),
#'                         col = "green", lwd = 1, lty = "dashed", show = TRUE)
#' }
#' @importFrom checkmate testDataFrame assertNames testList testTRUE testNull
#'   testClass assertIntegerish assertLogical
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols bind_rows
#' @export

geomPolygon <- function(anchor = NULL, window = NULL, template = NULL, features = 1,
                        vertices = NULL, regular = FALSE, show = FALSE, ...){

  # anchor = NULL; window = NULL; template = input; features = 1; vertices = NULL; regular = FALSE; show = TRUE
  
  # check arguments
  anchorIsDF <- testDataFrame(anchor, types = "numeric", any.missing = FALSE, min.cols = 2)
  if(anchorIsDF){
    colnames(anchor) <- tolower(colnames(anchor))
    assertNames(names(anchor), must.include = c("x", "y"), subset.of = c( "fid", "vid", "x", "y"))
    if(!"vid" %in% names(anchor)){
      anchor <- bind_cols(vid = seq_along(anchor$x), anchor)
    }
    if(!"fid" %in% names(anchor)){
      anchor <- bind_cols(fid = rep(1, times = length(anchor$x)), anchor)
    } 
    features <- length(unique(anchor$fid))
  }
  anchorIsGeom <- testClass(anchor, classes = "geom")
  if(anchorIsGeom){
    features <- length(unique(anchor@coords$fid))
  }
  windowExists <- !testNull(window)
  if(windowExists){
    assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
    colnames(window) <- tolower(colnames(window))
    assertNames(names(window), must.include = c("x", "y"))
  }
  templateExists <- !testNull(template)
  if(templateExists){
    assert(
      testClass(template, "RasterLayer"),
      testClass(template, "matrix")
    )
  }
  if(!anchorIsDF & !anchorIsGeom & !templateExists){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(regular)
  assertLogical(show)
  if(!anchorIsDF & !anchorIsGeom){
    assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE)
    if(length(vertices) != features){
      vertices <- rep(vertices, length.out = features)
    }
  } else{
    assertIntegerish(vertices, min.len = 1, lower = 2, any.missing = FALSE, null.ok = TRUE)
  }

  # get some raster properties
  if(templateExists){
    if(testClass(template, "RasterLayer")){
      tempName <- names(template)
      dims <- dim(template)
      projection <- crs(template, asText = TRUE)
    } else{
      tempName <- "layer"
      dims <- dim(template)
      projection <- NA
    }
  } else{
    tempName <- "layer"
    projection <- NA
  }

  # build a regular geometry
  nodes <- fids <- NULL
  out <- NULL
  for(i in 1:features){
    # if anchor does not exists, make it
    if(!anchorIsDF & !anchorIsGeom){

      if(regular){
        message("please click the polygons' center and the first vertex.")
        clicks <- 2
      } else{
        message(paste0("please click the ", vertices[i], " vertices."))
        clicks <- vertices[i]
      }
      theClicks <- locate(raster = template, samples = clicks, panel = tempName, silent = TRUE, show = FALSE, ...)
      window <- tibble(x = c(0, dims[2]),
                       y = c(0, dims[1]))
      tempAnchor <- tibble(fid = i, 
                           vid = 1:clicks,
                           x = theClicks$x,
                           y = theClicks$y)

    } else if(anchorIsGeom){
      if(!windowExists){
        window <- anchor@window
      }
      tempAnchor <- anchor@coords[anchor@coords$fid == i,]
    } else if(anchorIsDF){
      if(!windowExists){
        window <- tibble(x = c(min(anchor$x), max(anchor$x)),
                         y = c(min(anchor$y), max(anchor$y)))
      }
      tempAnchor <- anchor[anchor$fid == i, ]
    }

    if(regular){

      # trigonometry
      angle <- 360/vertices[i]
      angles <- seq(from = 90, to = 360-angle+90, by = angle)

      radius <- dist(tempAnchor[c(1:2),])
      cx <- tempAnchor$x[1] + radius*cos(rad(angles))
      cy <- tempAnchor$y[1] + radius*sin(rad(angles))
      theNodes <- tibble(fid = i, vid = 1:vertices, x = cx, y = cy)
      if(any(theNodes$x < min(window$x)) | any(theNodes$x > max(window$x)) | any(theNodes$y < min(window$y)) | any(theNodes$y > max(window$y))){
        window <- tibble(x = c(min(theNodes$x), max(theNodes$x)), y = c(min(theNodes$y), max(theNodes$y)))
      }

      temp <- new(Class = "geom",
                  type = "polygon",
                  coords = theNodes,
                  attr = tibble(fid = unique(theNodes$fid), n = length(unique(theNodes$vid))),
                  window = tibble(x = rep(c(min(window$x), max(window$x)), each = 2), y = c(min(window$y), max(window$y), max(window$y), min(window$y))),
                  scale = "absolute",
                  crs = as.character(projection),
                  history = list(paste0()))
      
      if(show){
        if(!any(names(listArgs()) == "new")){
          visualise(geom = temp, new = FALSE, ...)
        } else{
          visualise(geom = temp, ...)
        }
      }
      nodes <- bind_rows(nodes, theNodes)
      fids <- c(fids, length(unique(theNodes$fid)))
      
    } else{

      theNodes <- tempAnchor[c("fid", "vid", "x", "y")]

      temp <- new(Class = "geom",
                  type = "polygon",
                  coords = theNodes,
                  attr = tibble(fid = unique(theNodes$fid), n = length(unique(theNodes$vid))),
                  window = tibble(x = rep(c(min(window$x), max(window$x)), each = 2), y = c(min(window$y), max(window$y), max(window$y), min(window$y))),
                  scale = "absolute",
                  crs = as.character(projection),
                  history = list(paste0()))
      
      if(show){
        if(!any(names(listArgs()) == "new")){
          visualise(geom = temp, new = TRUE, ...)
        } else{
          visualise(geom = temp, ...)
        }
      }
      nodes <- bind_rows(nodes, theNodes)
      fids <- c(fids, length(unique(theNodes$vid)))
    }

  }
  out <- new(Class = "geom",
             type = "polygon",
             coords = nodes,
             attr = tibble(fid = unique(nodes$fid), n = fids),
             window = tibble(x = rep(c(min(window$x), max(window$x)), each = 2), y = c(min(window$y), max(window$y), max(window$y), min(window$y))),
             scale = "absolute",
             crs = as.character(projection),
             history = list(paste0("geometry was created as 'polygon'")))

  invisible(out)
}

#' @describeIn geomPolygon wrapper of geomPolygon where \code{vertices = 3} and
#'   \code{regular = TRUE}.
#' @export

geomTriangle <- function(anchor = NULL, window = NULL, template = NULL,
                         features = 1, show = FALSE, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(show)

  theGeom <- geomPolygon(anchor = anchor,
                         window = window,
                         template = template,
                         features = features,
                         vertices = 3,
                         regular = TRUE,
                         show = FALSE)
  
  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = theGeom, new = TRUE, ...)
    } else{
      visualise(geom = theGeom, ...)
    }
  }

  invisible(theGeom)
}

#' @describeIn geomPolygon wrapper of geomPolygon where \code{vertices = 4},
#'   \code{regular = TRUE} and a rotation by 45° about the centroid has been
#'   applied.
#' @export

geomSquare <- function(anchor = NULL, window = NULL, template = NULL,
                       features = 1, show = FALSE, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(show)

  theGeom <- geomPolygon(anchor = anchor,
                         window = window,
                         template = template,
                         features = features,
                         vertices = 4,
                         regular = TRUE,
                         show = FALSE)

  centroid <- colMeans(theGeom@coords[c("x", "y")])
  rotGeom <- gRotate(geom = theGeom,
                     angle = 45,
                     about = centroid)
  
  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = rotGeom, new = TRUE, ...)
    } else{
      visualise(geom = rotGeom, ...)
    }
  }
  invisible(rotGeom)
}

#' @describeIn geomPolygon wrapper of geomPolygon where \code{vertices = 2},
#'   \code{regular = FALSE} and the two complementing corners are derived from
#'   the two given opposing corners.
#' @export

geomRectangle <- function(anchor = NULL, window = NULL, template = NULL,
                          features = 1, show = FALSE, ...){

  anchorIsDF <- testDataFrame(anchor, types = "numeric", any.missing = FALSE, min.cols = 2)
  anchorIsGeom <- testClass(anchor, classes = "geom")
  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(show)

  if(anchorIsGeom){
    anchors <- anchor@coords
    window <- anchor@window
  } else{
    anchors <- anchor
  }
  newCoords <- NULL
  if(!any(colnames(anchors) == "fid")){
    anchors <- cbind(anchors, fid = 1)
  }
    
  for(i in unique(anchors$fid)){
    tempAnchor <- anchors[anchors$fid == i,]
    # get minimum and maximum value of x and y
    tempAnchor <- tibble(fid = i,
                         x = c(min(tempAnchor$x), max(tempAnchor$x)),
                         y = c(min(tempAnchor$y), max(tempAnchor$y)))
    # change positions of vertices, so that they follow a square
    tempAnchor <- tibble(fid = i,
                         x = rep(tempAnchor$x, each = 2),
                         y = c(tempAnchor$y, rev(tempAnchor$y)))
    newCoords <- rbind(newCoords, tempAnchor)
  }

  theGeom <- geomPolygon(anchor = newCoords,
                         window = window,
                         template = template,
                         features = features,
                         vertices = 4,
                         regular = FALSE,
                         show = FALSE)
  
  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = theGeom, new = TRUE, ...)
    } else{
      visualise(geom = theGeom, ...)
    }
  }
  invisible(theGeom)
}

#' @describeIn geomPolygon wrapper of geomPolygon where \code{vertices = 6} and
#'   \code{regular = TRUE}.
#' @export

geomHexagon <- function(anchor = NULL, window = NULL, template = NULL,
                        features = 1, show = FALSE, ...){

  if(is.null(anchor) & is.null(template)){
    stop("please provide either 'anchor' or 'template'.")
  }
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  assertIntegerish(features, len = 1, lower = 1)
  assertLogical(show)

  theGeom <- geomPolygon(anchor = anchor,
                         window = window,
                         template = template,
                         features = features,
                         vertices = 6,
                         regular = TRUE,
                         show = FALSE)
  
  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = theGeom, new = TRUE, ...)
    } else{
      visualise(geom = theGeom, ...)
    }
  }
  invisible(theGeom)
}

#' Create a geometry randomly
#'
#' This function creates a random geometry
#' @param type [\code{character(1)}]\cr Either one of the three main feature
#'   types \code{"point"}, \code{"line"} or \code{"polygon"}, or more
#'   specifically one of their subtypes, e.g. \code{"hexagon"}.
#' @template template
#' @param vertices [\code{integerish(1)}]\cr the number of vertices the geometry
#'   should have; only meaningful if \code{type} does not indicate the number of
#'   vertices already. If left at \code{NULL} the minimum number of vertices for
#'   the \code{geom} type, i.e. 1 for \code{point}, 2 for \code{line} and 3 for
#'   \code{polygon}
#' @param show [\code{logical(1)}]\cr should the geometry be plotted
#'   (\code{TRUE}) or should it not be plotted (\code{FALSE}, default)? In case
#'   \code{template} is set, it is automatically \code{TRUE}.
#' @param ... [various]\cr graphical parameter, in case \code{show = TRUE}; see
#'   \code{\link{gpar}}.
#' @family shapes
#' @examples
#' input <- matrix(nrow = 100, ncol = 100, data = 0)
#'
#' # create a random geometry with five vertices
#' set.seed(1)
#' someGeom <- geomRand(type = "polygon", vertices = 5)
#' visualise(geom = someGeom)
#'
#' # in case template is given, this serves as source for the window extent
#' someGeom <- geomRand(template = input, col = "green", show = TRUE)
#' @export

geomRand <- function(type = "point", template = NULL, vertices = NULL, 
                     show = FALSE, ...){
  
  assertSubset(type, choices = c("point", "line", "rectangle", "square", "polygon", "spline", "ellipse", "circle", "triangle", "hexagon"))
  templateExists <- !testNull(template)
  if(templateExists){
    isRaster <- testClass(template, "RasterLayer")
    isMatrix <- testClass(template, "matrix")
    if(!isRaster & !isMatrix){
      stop("please provide either a RasterLayer or a matrix as 'template'.")
    }
  }
  assertIntegerish(vertices, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertLogical(show)

  if(type %in% "point"){
    if(is.null(vertices)){
      vertices <- 1
    }
    outType  <- type
    anchor <- tibble(fid = 1:vertices,
                     vid = 1:vertices,
                     x = runif(vertices),
                         y = runif(vertices))
  # } else if(type %in% c("line", "spline")){
  #   if(is.null(vertices)){
  #     vertices <- 2
  #   }
  #   outType <- "line"
  #   
  } else{
    if(is.null(vertices)){
      vertices <- 3
    }
    outType <- "polygon"
    anchor <- tibble(fid = 1,
                     vid = 1,
                     x = runif(vertices),
                     y = runif(vertices))
  }

  if(templateExists){
    window <- getExtent(template)
  } else{
    window <- tibble(x = c(0, 1),
                     y = c(0, 1))
  }
  
  theGeom <- new(Class = "geom",
                 type = outType,
                 coords = anchor,
                 attr = tibble(fid = unique(anchor$fid), n = 1),
                 window = window,
                 scale = "relative",
                 crs = as.character(NA),
                 history = list(paste0("geometry was created randomly")))
  
  if(templateExists){
    theGeom <- gScale(theGeom, to = "absolute")
  }
  
  if(show){
    if(!any(names(listArgs()) == "new")){
      visualise(geom = theGeom, new = TRUE, ...)
    } else{
      visualise(geom = theGeom, ...)
    }
  }
  invisible(theGeom)
}

#' Create a regular tiling geometry
#'
#' @param window [\code{data.frame(1)}]\cr the origin (lower left corner) and
#'   the maximum value (upper right corner) of the tiling.
#' @param cells [\code{integerish(2)}]\cr number of tiles in \code{x} and
#'   \code{y} dimension.
#' @param crs [\code{character(1)}]\cr corrdinate reference system of the
#'   object in proj4 notation.
#' @param tiling [\code{character(1)}]\cr pattern of the tiling. Possible
#'   options are \code{"rectangular"} (default), \code{"hexagonal"},
#'   \code{"triangular"}.
#' @param centroids [\code{logical(1)}]\cr should the centroids of the tiling be
#'   returned (\code{TRUE}) or should the tiling be returned (\code{FALSE},
#'   default)?
#' @return An invisible \code{geom}.
#' @examples
#' # create grid for GFC data
#' gfcWindow <- data.frame(x = c(-180, 180),
#'                         y = c(-60, 80))
#' tiles_gfc <- geomTiles(window = gfcWindow, cells = c(36, 14), crs = projs$longlat)
#'
#' # create grid for MODIS data
#' modWindow <- data.frame(x = c(-20015109.354, 20015109.354),
#'                         y = c(-10007554.677, 10007554.677))
#' tiles_modis <- geomTiles(window = modWindow, cells = c(36, 18), crs = projs$sinu)
#'
#' # create grid for the sentinel data
#' #sntWindow <- data.frame(x = c(),
#' #                        y = c())
#' #tiles_sentinel <- geomTiles(window = sntWindow, cells = c(), crs = projs$utm)
#' @importFrom checkmate testDataFrame assertNames testClass testIntegerish
#'   assertDataFrame assertNames assertCharacter assertSubset assertLogical
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export

geomTiles <- function(window = NULL, cells = NULL, crs = NULL,
                      tiling = "rectangular", centroids = FALSE){

  # check arguments
  assertIntegerish(cells, len = 2, any.missing = FALSE)
  assertDataFrame(window, types = "numeric", any.missing = FALSE, ncols = 2, null.ok = TRUE)
  colnames(window) <- tolower(colnames(window))
  assertNames(names(window), must.include = c("x", "y"))
  assertCharacter(crs, fixed = "+proj", null.ok = TRUE)
  assertCharacter(tiling, ignore.case = TRUE, any.missing = FALSE, len = 1)
  assertSubset(tiling, choices = c("rectangular", "hexagonal", "triangular"))
  assertLogical(centroids)

  if(!is.null(crs)){
    projection <- crs
  } else{
    projection <- NA
  }

  if(tiling == "rectangular"){

    xDist <- (abs(min(window$x)) + abs(max(window$x)))/cells[1]
    yDist <- (abs(min(window$y)) + abs(max(window$y)))/cells[2]
    if(round(xDist, 3) != round(yDist, 3)){
      stop("a tiling of ", cells[1], " by ", cells[2], " cells would yield irregular cells with the given 'window'.\n Please adapt either 'cells' or 'window'.")
    }

    # determine centroids
    xCentroids <- seq(min(window$x) + xDist/2, max(window$x), xDist)
    yCentroids <- seq(min(window$y) + yDist/2, max(window$y), yDist)
    cntrds <- tibble(fid = seq(1:(cells[1]*cells[2])),
                     x = rep(xCentroids, times = length(yCentroids)),
                     y = rep(yCentroids, each = length(xCentroids)))

    angle <- 360/4
    angles <- seq(from = 45, to = 360-angle+45, by = angle)
    radius <- sqrt(xDist**2 + yDist**2)/2

  } else if(tiling == "hexagonal"){

    height <- (abs(min(window$y)) + abs(max(window$y)))/cells[2]
    width <- 2 * (height / sqrt(3))

    # determine centroids
    xC1 <- seq(min(window$x), max(window$x) + width/2, by = 3/2*width)
    xC2 <- seq(min(window$x) + 3/4*width, max(window$x), by = 3/2*width)
    if(length(xC1) > cells[1]/2+1){
      xC1 <- xC1[c(1:(cells[1]/2+1))]
      xC2 <- xC2[c(1:(cells[1]/2))]
    }
    yC1 <- seq(min(window$y), max(window$y), by = height)
    yC2 <- seq(min(window$y) + height/2, max(window$y), by = height)

    cntrds <- tibble(fid = seq(1:(length(yC1)*length(xC1) + length(yC2)*length(xC2))),
                     x = c(rep(xC1, times = length(yC1)), rep(xC2, times = length(yC2))),
                     y = c(rep(yC1, each = length(xC1)), rep(yC2, each = length(xC2))))

    angle <- 360/6
    angles <- seq(from = 0, to = 360-angle, by = angle)
    radius <- width/2

  } else if(tiling == "triangular"){
  }

  if(!centroids){
    nodes <- NULL
    for(i in seq_along(cntrds$fid)){
      cx <- round(cntrds$x[i] + radius*cos(rad(angles)))
      cy <- round(cntrds$y[i] + radius*sin(rad(angles)))
      theNodes <- tibble(fid = i, vid = 1:length(angles), x = cx, y = cy)
      nodes <- bind_rows(nodes, theNodes)
    }
    theType <- "polygon"
  } else{
    nodes <- cntrds
    theType <- "point"
  }

  window <- tibble(x = rep(window$x, each = 2),
                   y = c(window$y, rev(window$y)))

  theTiles <- new(Class = "geom",
                  type = theType,
                  coords = nodes,
                  attr = tibble(fid = unique(nodes$fid), n = 1),
                  window = window,
                  scale = "absolute",
                  crs = as.character(projection),
                  history = list(paste0("tiled geometry of type '", theType, "' was created.")))

  invisible(theTiles)
}