#' Group geometries
#'
#' \code{gGroup} assigns the vertices of a \code{geom} into groups of features.
#' @template geom
#' @param index [\code{integerish(.)}]\cr a vector with a value for each vertex,
#'   according to which to group.
#' @param distance [\code{numeric(1)}]\cr specific distance of two vertices
#'   below which they will be included in the same cluster; must be within the
#'   range of \code{extent}.
#' @param clusters [\code{integerish(1)}]\cr the number of clusters for
#'   \code{\link{kmeans}} clustering.
#' @param ...  [various]\cr additional arguments either to
#'   \code{\link[stats]{hclust}} or to \code{\link[stats]{kmeans}}.
#' @details Only one of the three arguments \code{index}, \code{distance} or
#'   \code{clusters} need to be set, as grouping is only carried out by one of
#'   them.
#'
#'   In case the geom had an attribute table, this has to be redefined by
#'   default because it is impossible to determine how these attributes should
#'   be reattributed without external information.
#' @return \code{geom} with grouped coordinates.
#' @examples
#' coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
#'                      y = c(40, 40, 60, 70, 10, 20, 40),
#'                      id = c(1, 1, 1, 1, 2, 2, 2))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geomPolygon(anchor = coords, window = window, col = "goldenrod1", show = TRUE)
#'
#' grouped <- gGroup(geom = aGeom, distance = 40)
#' visualise(geom = grouped)
#' @importFrom checkmate testList assertNames assertDataFrame
#' @importFrom methods new
#' @importFrom stats kmeans
#' @export

gGroup <- function(geom, index = NULL, distance = NULL, clusters = NULL, ...){
  
  assertClass(geom, classes = "geom")
  assertIntegerish(index, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  assertNumeric(distance, finite = TRUE, null.ok = TRUE)
  assertIntegerish(clusters, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  if(is.null(distance) & is.null(index) & is.null(clusters)){
    stop("please provide either 'distance', 'index' or 'clusters'.")
  }
  
  coords <- geom@coords
  toGroup <- coords[c("x", "y")]
  
  if(!is.null(index)){
    newId <- rep(index, length.out = dim(toGroup)[1])
  }
  if(!is.null(distance)){
    temp <- dist(toGroup)
    h <- hclust(temp, ...)
    newId <- cutree(h, h=distance)
  }
  if(!is.null(clusters)){
    temp <- kmeans(toGroup, centers = clusters, ...)
    newId <- temp$cluster
  }
  
  temp <- cbind(toGroup, id = newId, fid = newId)
  
  out <- new(Class = "geom",
             type = geom@type,
             coords = temp,
             attr = data.frame(id = unique(newId), n = 1),
             window = geom@window,
             scale = geom@scale,
             crs = geom@crs,
             history = list(paste0("geometry values were regrouped")))
  
  return(out)
}

#' Rotate geometries
#'
#' Rotate \code{geom}s by a certain angle about a center
#' @template geom
#' @param angle [\code{numeric(1)}]\cr the counter-clockwise angle by which
#'   \code{geom} shall be rotated.
#' @param about [\code{numeric(2)}]\cr the point about which \code{geom} shall
#'   be rotated.
#' @param id [\code{integerish(.)}]\cr vector of ids that should be rotated.
#' @return Rotated \code{geom}.
#' @examples
#' coords <- data.frame(x = c(30, 60, 60, 40, 10, 40, 20),
#'                      y = c(40, 40, 60, 70, 10, 20, 40),
#'                      id = c(1, 1, 1, 1, 2, 2, 2))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geomPolygon(anchor = coords, window = window,
#'                      col = "blue", show = TRUE)
#'
#' rotatedGeom <- gRotate(geom = aGeom, angle = 90, about = c(40, 40))
#' visualise(geom = rotatedGeom)
#'
#' # rotate single objects
#' rotatedTriangle <- gRotate(geom = aGeom, angle = -180, about = c(30, 40), id = 2)
#' visualise(geom = rotatedTriangle, col = "goldenrod1")
#'
#' # rotate different objects about different centers by different angles
#' rotateMore <- gRotate(geom = aGeom,
#'                       angle = list(90, -180),
#'                       about = list(c(40, 40), c(30, 40)))
#' visualise(geom = rotateMore, col = "deeppink")
#' @importFrom checkmate assertNames testList testNumeric assertNumeric
#' @importFrom methods new
#' @export

gRotate <- function(geom, angle, about = c(0, 0), id = NULL){
  
  assertClass(geom, classes = "geom")
  angleIsList <- testList(angle, types = "numeric", any.missing = FALSE)
  angleIsNumeric <- testNumeric(angle, any.missing = FALSE, lower = -360, upper = 360, len = 1)
  aboutIsList <- testList(about, types = "numeric", any.missing = FALSE)
  aboutIsNumeric <- testNumeric(about, any.missing = FALSE, len = 2)
  assert(angleIsList, angleIsNumeric)
  assert(aboutIsList, aboutIsNumeric)
  if(aboutIsNumeric){
    about <- list(about)
  }
  if(angleIsNumeric){
    angle <- list(angle)
  }
  existsID <- !is.null(id)
  
  coords <- geom@coords
  ids <- unique(coords$id)
  if(existsID){
    doRotate <- ids %in% id
  } else{
    doRotate <- rep(TRUE, length(ids))
  }
  
  if(length(angle) != length(ids)){
    angle <- rep(angle, length.out = length(ids))
  }
  if(length(about) != length(ids)){
    about <- rep(about, length.out = length(ids))
  }
  
  digits <- getOption("digits")
  
  # out <- geom
  temp <- NULL
  for(i in seq_along(ids)){
    tempCoords <- coords[coords$id == ids[i],]
    
    if(doRotate[i]){
      tempAngle <- angle[[i]]
      tempAbout <- about[[i]]
      xVals <- tempCoords$x
      yVals <- tempCoords$y
      
      if(!all(tempAbout == c(0, 0))){
        offset <- c(0, 0) - tempAbout
        xVals <- xVals + offset[1]
        yVals <- yVals + offset[2]
      }
      
      tempCoords$x <- round(xVals * cos(rad(tempAngle)) - yVals * sin(rad(tempAngle)), digits)
      tempCoords$y <- round(xVals * sin(rad(tempAngle)) + yVals * cos(rad(tempAngle)), digits)
      
      if(!all(tempAbout == c(0, 0))){
        tempCoords$x <- tempCoords$x - offset[1]
        tempCoords$y <- tempCoords$y - offset[2]
      }
    }
    temp <- rbind(temp, tempCoords)
    
  }
  out <- new(Class = "geom",
             type = geom@type,
             coords = temp,
             attr = geom@attr,
             window = geom@window,
             scale = geom@scale,
             crs = geom@crs,
             history = list(paste0("geometry was rotated")))
  
  return(out)
}

#' Scale geometries
#'
#' Relative coordinates are required to work with grobs of \code{geom}s, absolute
#' coordinates are required to create spatial objects thereof.
#' @template geom
#' @param range [\code{list(2)}]\cr integerish vector of length two for \code{x}
#'   and \code{y}.
#' @param to [\code{character(1)}]\cr the scale to which the coordinates should
#'   be transformed; possible are \code{"relative"} and \code{"absolute"};
#'   ignored in case \code{range != NULL}.
#' @return Scaled \code{geom}.
#' @examples
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70),
#'                      id = 1)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geomPolygon(anchor = coords, window = window, col = "blue")
#'
#' # change to relative scale and back to absolute
#' (relCoords <- gScale(geom = aGeom, to = "relative"))
#' gScale(geom = relCoords, to = "absolute")
#'
#' gScale(geom = aGeom, range = list(x = c(0, 100), y = c(10, 90)))
#'
#' @importFrom checkmate testList assertNames
#' @importFrom methods new
#' @export

gScale <- function(geom, range = NULL, to = "relative"){
  
  assertClass(geom, classes = "geom")
  existsRange <- testTRUE(!is.null(range))
  if(existsRange){
    assertList(range, len = 2, names = "named")
    assertNames(names(range), permutation.of = c("x", "y"))
    assertIntegerish(range$x, len = 2, any.missing = FALSE)
    assertIntegerish(range$y, len = 2, any.missing = FALSE)
    to <- "relative"
  } else{
    to <- match.arg(to, c("relative", "absolute"))
  }

  coords <- geom@coords
  window <- geom@window

  out <- NULL
  if(to == "relative"){
    if(existsRange){
      rangeX <- range$x
      rangeY <- range$y
    } else{
      rangeX <- c(0, 1)
      rangeY <- c(0, 1)
    }
    minX <- min(window$x)
    maxX <- max(window$x)
    minY <- min(window$y)
    maxY <- max(window$y)
  } else{
    rangeX <- c(min(window$x), max(window$x))
    rangeY <- c(min(window$y), max(window$y))
    minX <- 0
    maxX <- 1
    minY <- 0
    maxY <- 1
  }
  
  temp <- coords
  temp$x <- (temp$x - minX) * (rangeX[2] - rangeX[1]) / (maxX - minX) + rangeX[1]
  temp$y <- (temp$y - minY) * (rangeY[2] - rangeY[1]) / (maxY - minY) + rangeY[1]
  out <- rbind(out, temp)
  
  if(existsRange){
    window <- as.data.frame(range)
    to <- "absolute"
  }
  out <- new(Class = "geom",
             type = geom@type,
             coords = out,
             attr = geom@attr,
             window = window,
             scale = to,
             crs = geom@crs,
             history = list(paste0("geometry values were scaled to '", to, "'")))
  
  return(out)
}

#' Transform geometry to grob
#'
#' A \code{\link{grob}} (graphical object) is the grid-package representation of
#' a \code{geom} and is used for plotting.
#' @template geom
#' @template theme
#' @param ... instead of providing a \code{theme}, you can also determine
#'   specific graphic parameters (see \code{\link{gpar}}) separately.
#' @return Depending on the provided geometry either a \code{\link{pointsGrob}},
#'   \code{\link{linesGrob}}, \code{\link{polylineGrob}} or a
#'   \code{\link{polygonGrob}}.
#' @examples
#' coords <- data.frame(x = c(40, 70, 70, 50, 40, 60, 70, 40, 60, 
#'                            40, 10, 20, 30, 30, 20, 50, 40, 10, 20),
#'                      y = c(40, 40, 60, 70, 40, 20, 40, 10, 20, 
#'                            40, 20, 20, 50, 40, 40, 70, 40, 20, 60),
#'                      id = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 
#'                             3, 3, 3, 4, 4, 4, 4, 4, 4, 4),
#'                      fid = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 
#'                              3, 3, 3, 4, 4, 4, 5, 5, 5, 5))
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geomPolygon(anchor = coords, window = window)
#'
#' aGrob <- gToGrob(geom = aGeom)
#' str(aGrob)
#' @importFrom checkmate assertNames assertSubset assertList
#' @importFrom grid gpar unit pointsGrob pathGrob polylineGrob clipGrob
#' @export

gToGrob <- function(geom, theme = NULL, ...){
  
  assertClass(geom, classes = "geom")
  assertClass(x = theme, classes = "rtTheme", null.ok = TRUE)
  if(is.null(theme)){
    theme <- rtTheme
  }
  
  # scale it to relative, if it's not
  if(geom@scale == "absolute"){
    outGeom <- gScale(geom = geom, to = "relative")
  } else{
    outGeom <- geom
  }
  
  featureType <- geom@type
  coords <- outGeom@coords
  
  attr <- getTable(x = geom)
  scaleTo <- eval(parse(text = paste0(theme@geom$scale$to)), envir = attr)
  pars <- scaleParameters(attr = attr, params = theme@geom)
  
  if(featureType %in% c("point")){

    geomGrob <- pointsGrob(x = coords$x,
                           y = coords$y,
                           pch = theme@geom$pointsymbol,
                           size = unit(theme@geom$pointsize, "char"),
                           gp = gpar(
                             col = pars$line,
                             fill = pars$fill,
                             ...))
    
  } else if(featureType %in% "line"){
    
    geomGrob <- polylineGrob(x = coords$x,
                             y = coords$y,
                             id = as.numeric(as.factor(tempCoords$fid)),
                             gp = gpar(col = pars$line,
                                       lty = pars$linetype,
                                       lwd = pars$linewidth,
                                       ...))
    
  } else if(featureType %in% c("polygon")){
    
    geomGrob <- NULL
    for(i in seq_along(scaleTo)){
      tempCoords <- coords[eval(parse(text = paste0("coords$", theme@geom$scale$to))) == scaleTo[i],]
      if(i == 1){
        geomGrob <- pathGrob(x = tempCoords$x,
                             y = tempCoords$y,
                             id = as.numeric(as.factor(tempCoords$fid)),
                             rule = "evenodd",
                             gp = gpar(
                               col = pars$line[i],
                               fill = pars$fill[i],
                               lty = pars$linetype[i],
                               lwd = pars$linewidth[i],
                               ...))
      } else{
        geomGrob <- gList(geomGrob, 
                          pathGrob(x = tempCoords$x,
                                   y = tempCoords$y,
                                   id = as.numeric(as.factor(tempCoords$fid)),
                                   rule = "evenodd",
                                   gp = gpar(
                                     col = pars$line[i],
                                     fill = pars$fill[i],
                                     lty = pars$linetype[i],
                                     lwd = pars$linewidth[i],
                                     ...)))
      }

    }

  }
  return(geomGrob)
  
}

#' Transform geometry to raster
#'
#' An object of class \code{RasterLayer} is the raster-package representation of
#' a \code{geom}.
#' @template geom
#' @param negative [\code{logical(1)}]\cr should the area covered by \code{geom}
#'   be set to 0 (\code{TRUE}) or should it be set to 1 (\code{FALSE}, default)?
#' @param res [\code{numeric(2)}]\cr resolution in x and y direction.
#' @param crs [\code{proj4string(1)}]\cr corrdinate reference system of the
#'   object.
#' @return a binary \code{\link{raster}} with the dimensions of the reference
#'   window of \code{geom} and the resolution \code{res}.
#' @examples
#' coords <- data.frame(x = c(40, 70, 70, 50),
#'                      y = c(40, 40, 60, 70),
#'                      id = 1)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' aGeom <- geomPolygon(anchor = coords, window = window)
#'
#' aRaster <- gToRaster(geom = aGeom)
#' visualise(gridded = aRaster, geom = aGeom, col = "deeppink")
#'
#' negRaster <- gToRaster(geom = aGeom, negative = TRUE)
#' visualise(gridded = negRaster, geom = aGeom, col = "deeppink")
#' @importFrom methods new
#' @importFrom raster raster extent<-
#' @importFrom sp proj4string<- spTransform
#' @export

gToRaster <- function(geom, negative = FALSE, res = c(1, 1), crs = NULL){
  
  assertClass(geom, classes = "geom")
  assertLogical(negative)
  assertNumeric(res, len = 2, finite = TRUE)
  assertCharacter(crs, fixed = "+proj", null.ok = TRUE)
  if(!is.null(crs)){
    targetCRS <- crs
  } else{
    targetCRS <- NA
  }
  if(!is.na(geom@crs)){
    sourceCRS <- geom@crs
  } else{
    sourceCRS <- NA
  }
  
  theWindow <- getWindow(geom)
  extCols <- round(c(min(theWindow$x, na.rm = TRUE), max(theWindow$x, na.rm = TRUE))/res[1])
  outCols <- round(max(extCols, na.rm = TRUE) - min(extCols, na.rm = TRUE))
  extRows <- round(c(min(theWindow$y, na.rm = TRUE), max(theWindow$y, na.rm = TRUE))/res[2])
  outRows <- round(max(extRows, na.rm = TRUE) - min(extRows, na.rm = TRUE))
  
  temp <- matrix(data = 0, ncol = outCols, nrow = outRows)
  coords <- geom@coords[c("x", "y")]
  coords[,1] <- round(coords[,1]/res[1])
  coords[,2] <- round(coords[,2]/res[2])
  vertices <- as.matrix(coords)
  if(!any(theWindow$x == 0)){
    vertices[,1] <- vertices[,1] - min(vertices[,1], na.rm = TRUE)
  }
  if(!any(theWindow$y == 0)){
    vertices[,2] <- vertices[,2] - min(vertices[,2], na.rm = TRUE)
  }
  if(any(coords[dim(coords)[1],] != coords[1,])){
    vertices <- rbind(vertices, vertices[1,])
  }
  geomRaster <- matInGeomC(mat = temp, geom = vertices, negative = negative)
  # out <- raster(geomRaster, xmn = min(coords[,1])*res[1], xmx = max(coords[,1])*res[1], ymn = min(coords[,2])*res[2], ymx = max(coords[,2])*res[2], crs = CRS(theCRS))
  out <- raster(geomRaster, xmn = 0, xmx = outCols, ymn = 0, ymx = outRows, crs = as.character(sourceCRS))
  extent(out) <- extent(extCols[1]*res[1], extCols[2]* res[1], extRows[1]*res[2], extRows[2]*res[2])

  out@history <- c(geom@history, list(paste0("geometry was transformed to a raster")))

  if(!is.na(targetCRS)){
    out <- setCRS(x = out, crs = targetCRS)
  }

  return(out)
}

#' Transform geometry to spatial object
#'
#' An object of class \code{Spatial*} is the sp-package representation of a
#' \code{geom}.
#' @template geom
#' @param crs [\code{proj4string(1)}]\cr corrdinate reference system of the
#'   object.
#' @return Depending on the provided geometry either a
#'   \code{\link{SpatialPointsDataFrame}}, \code{\link{SpatialLinesDataFrame}}
#'   or \code{\link{SpatialPolygonsDataFrame}} object.
#' @examples
#' require(magrittr)
#' somePoints <- data.frame(X = c(5027609, 5190599, 5326537, 5222810,
#'                          5234735, 5281527, 5189955, 5041066),
#'                          Y = c(3977612, 3971119, 4028167, 3997230,
#'                          4060164, 4117856, 4118207, 4062838),
#'                          id = c(1:8))
#'
#' pointsGeom <- geomPoint(anchor = somePoints)
#' polyGeom <- gGroup(geom = pointsGeom, index = c(rep(1, 8))) %>%
#'   geomPolygon()
#'
#' (spPoints <- gToSp(geom = pointsGeom, crs = projs$laea))
#' (spPolygon <- gToSp(geom = polyGeom, crs = projs$laea))
#' @importFrom checkmate assertClass assertCharacter
#' @importFrom sp CRS SpatialPoints SpatialPointsDataFrame Polygon Polygons
#'   SpatialPolygons SpatialPolygonsDataFrame proj4string<- spTransform
#' @export

gToSp <- function(geom, crs = NULL){

  assertClass(geom, classes = "geom")
  assertCharacter(crs, fixed = "+proj", null.ok = TRUE)
  if(is.null(crs) & is.na(geom@crs)){
    target_crs <- as.character(NA)
  } else if(!is.null(crs)){
    target_crs <- crs
  } else if(!is.na(geom@crs)){
    target_crs <- geom@crs
  }
  if(is.na(geom@crs)){
    source_crs <- as.character(NA)
  } else{
    source_crs <- geom@crs
  }

  featureType <- geom@type
  coords <- geom@coords
  id <- unique(coords$id)

  if(featureType %in% c("point")){

    temp <- coords[c("x", "y")]
    geomSp <- SpatialPoints(temp, proj4string = CRS(source_crs))
    geomSp <- SpatialPointsDataFrame(geomSp, data = data.frame(id = seq_along(geomSp)), match.ID = FALSE)

  # } else if(featureType %in% c("line")){
  #
  #   temp <- list()
  #   # go through distinct ids and check whether the last coordinate is equal to the first.
  #   for(i in seq_along(id)){
  #     thePoly <- coords[c(1, 2)][coords$id == id[i],]
  #     if(!all(thePoly[1,] == thePoly[dim(thePoly)[1],])){
  #       thePoly <- rbind(thePoly, thePoly[1,])
  #     }
  #
  #     # put togehter the 'Polygons' list
  #     temp <- c(temp, Polygons(list(Polygon(thePoly)), id[i]))
  #   }
  #
  #   geomSp <- SpatialLines(temp, proj4string = CRS(theCRS))
  #   geomSp <- SpatialLinesDataFrame(geomSp, data = data.frame(id = seq_along(geomSp)), match.ID = FALSE)

  } else if(featureType %in% c("polygon")){

    temp <- list()
    # go through distinct ids and check whether the last coordinate is equat to the first.
    for(i in seq_along(id)){
      thePoly <- coords[c("x", "y")][coords$id == id[i],]
      if(!all(thePoly[1,] == thePoly[dim(thePoly)[1],])){
        thePoly <- rbind(thePoly, thePoly[1,])
      }

      # put togehter the 'Polygons' list
      temp <- c(temp, Polygons(list(Polygon(thePoly)), id[i]))
    }

    # make a SpatialPolygon out of that
    geomSp <- SpatialPolygons(temp, proj4string = CRS(source_crs))
    geomSp <- SpatialPolygonsDataFrame(geomSp, data = data.frame(id = seq_along(geomSp)), match.ID = FALSE)

  }

  if(is.na(source_crs)){
    proj4string(geomSp) <- target_crs
  } else{
    geomSp <- spTransform(geomSp, target_crs)
  }

  return(geomSp)
}

#' Derive a \code{geom} from other spatial objects
#'
#' See \code{\link{geom-class}} for details on differences between objects of
#' class \code{geom} and other spatial classes.
#' @param input [various]\cr the spatial object; currently defined are ojects of
#'   class \code{sp} and \code{sf}.
#' @param window [\code{data.frame(1)}]\cr reference window, i.e. minimum and
#'   maximum values of the \code{x}- and \code{y}-dimension within which the
#'   \code{geom} should dwell.
#' @return a \code{geom} of the type that comes closest to the type of the
#'   input.
#' @examples
#' require(sp)
#' pol1 <- Polygon(cbind(c(4, 4, 5, 7, 4),c(5, 3, 2, 5, 5)))
#' pol2 <- Polygon(cbind(c(4.5, 5.5, 5.5, 5, 4.5, 4.5),
#'                       c(4.5, 4, 3.5, 3, 3, 4)),
#'                       hole = TRUE)
#' poly <- Polygons(list(pol1, pol2), "1")
#' spPoly <- SpatialPolygons(list(poly))
#' plot(spPoly, col = "goldenrod")
#'
#' myGeom <- gFrom(input = spPoly)
#' visualise(geom = myGeom)
#'
#' @importFrom checkmate assertClass testDataFrame
#' @importFrom sp proj4string
#' @importFrom methods as
#' @export

gFrom <- function(input, window = NULL){

  # check arguments
  existsSp <- testClass(input, classes = "Spatial")
  # existsSf <- testClass(input, classes = "sf")
  existsWindow <- testDataFrame(window, types = "numeric", any.missing = FALSE, nrows = 2, ncols = 2)
  # if(!existsSp & !existsSf){
  #   stop("I can recently only transform objects of packages 'sp' and 'sf' to class 'geom'.")
  # }

  if(existsSp){

    theCoords <- NULL
    theData <- NULL
    sourceClass <- class(input)[1]
    prev <- 0
    
    # in case we deal with an "exotic" class, transform to the nearest "normal" class
    if(sourceClass %in% c("SpatialGrid")){
      input <- as(input, "SpatialPolygons")
    } else if(sourceClass %in% "SpatialGridDataFrame"){
      input <- as(input, "SpatialPolygonsDataFrame")
    } else if(sourceClass %in% "SpatialPixels"){
      input <- as(input, "SpatialPoints")
    } else if(sourceClass %in% "SpatialPixelsDataFrame"){
      input <- as(input, "SpatialPointsDataFrame")
    }
    sourceClass <- class(input)[1]

    if(sourceClass %in% c("SpatialPoints", "SpatialPointsDataFrame")){
      type <- "point"

      if(sourceClass %in% "SpatialPointsDataFrame"){
        theData <- data.frame(id = seq_along(input@coords[,1]))
        theData <- cbind(theData, input@data)
      } else{
        theData <- data.frame(id = seq_along(input@coords[,1]))
      }
      theCoords <- data.frame(id = seq_along(input@coords[,1]), input@coords)
      colnames(theCoords) <- c("id", "x", "y")
      rownames(theData) <- NULL
      
    } else if(sourceClass %in% c("SpatialMultiPoints", "SpatialMultiPointsDataFrame")){
      type <- "point"
      
      for(i in seq_along(input@coords)){
        tempCoords <- data.frame(id = i,
                                 fid = prev + seq_along(input@coords[[i]][,1]),
                                 x = input@coords[[i]][,1],
                                 y = input@coords[[i]][,2])
        theCoords <- rbind(theCoords, tempCoords)
        
        if(sourceClass %in% "SpatialMultiPointsDataFrame"){
          tempData <- data.frame(i, length(input@coords[[i]][,1]), input@data[i,])
          theData <- rbind(theData, tempData)
          otherNames <- colnames(input@data)
        } else{
          tempData <- data.frame(i, length(input@coords[[i]][,1]))
          theData <- rbind(theData, tempData)
          otherNames <- NULL
        }
        prev <- prev + length(input@coords[[i]][,1])
      }
      colnames(theData) <- c("id", "n", otherNames)
      rownames(theData) <- NULL
      
    } else if(sourceClass %in% c("SpatialLines", "SpatialLinesDataFrame")){
      type <- "line"
      
      for(i in seq_along(input@lines)){
        theLines <- input@lines[[i]]
        for(j in seq_along(theLines@Lines)){
          theLine <- theLines@Lines[[j]]
          
          tempCoords <- data.frame(id = i,
                                   fid = prev + j,
                                   x = theLine@coords[,1],
                                   y = theLine@coords[,2])
          theCoords <- rbind(theCoords, tempCoords)
        }
        
        if(sourceClass %in% "SpatialLinesDataFrame"){
          tempData <- data.frame(i, length(theLines@Lines), input@data[i,])
          theData <- rbind(theData, tempData)
          otherNames <- colnames(input@data)
        } else{
          theData <- rbind(theData, data.frame(i, length(theLines@Lines)))
          otherNames <- NULL
        }
        prev <- prev + length(theLines@Lines)
      }
      colnames(theData) <- c("id", "n", otherNames)
      rownames(theData) <- NULL
      
    } else if(sourceClass %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")){
      type <- "polygon"
      
      for(i in seq_along(input@polygons)){
        thePolys <- input@polygons[[i]]
        for(j in seq_along(thePolys@Polygons)){
          thePoly <- thePolys@Polygons[[j]]
          
          if(thePoly@hole){
            fidID <- j-1
          } else{
            fidID <- j
          }
          tempCoords <- data.frame(id = i,
                                   fid = prev + j,
                                   x = thePoly@coords[,1],
                                   y = thePoly@coords[,2])
          theCoords <- rbind(theCoords, tempCoords)
        }
        
        if(sourceClass %in% "SpatialPolygonsDataFrame"){
          tempData <- data.frame(i, length(thePolys@Polygons), input@data[i,])
          theData <- rbind(theData, tempData)
          otherNames <- colnames(input@data)
        } else{
          theData <- rbind(theData, data.frame(i, length(thePolys@Polygons)))
          otherNames <- NULL
        }
        prev <- prev + length(thePolys@Polygons)
      }
      colnames(theData) <- c("id", "n", otherNames)
      rownames(theData) <- NULL
      
    }

    sourceCrs <- proj4string(input)
    bbox <- data.frame(input@bbox)
    if(!existsWindow){
      theWindow <- data.frame(x = rep(c(bbox$min[1], bbox$max[1]), each = 2),
                              y = c(bbox$min[2], bbox$max[2], bbox$max[2], bbox$min[2]))
    } else{
      theWindow <- window
    }



  } #else if(existsSf){

  # sourceClass <-
  # sourceCrs <-
  #

  #}

  theGeom <- new(Class = "geom",
                 type = type,
                 coords = theCoords,
                 attr = theData,
                 window = theWindow,
                 scale = "absolute",
                 crs = as.character(sourceCrs),
                 history = list(paste0("geometry was created from an object of class '", sourceClass, "'")))

  return(theGeom)

}