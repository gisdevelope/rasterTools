#' Binarise the values in a raster
#'
#' Transform a raster to binary form.
#' @template obj
#' @param thresh [\code{integerish(1)}]\cr Value above which the cell will be set
#'   to 1, below which it will be set to 0.
#' @param match [\code{integerish(.)}]\cr One or more values which will be set to
#'   1, while the remaining values will be set to 0.
#' @return A binary \code{RasterLayer} of the same dimension as \code{obj}.
#' @details A binary object contains only values 1 (foreground) and 0
#'   (background).
#' @family operators to modify cell values
#' @examples
#' input <- rtData$continuous
#' visualise(rBinarise(input, thresh = 30))
#'
#' visualise(rBinarise(input, match = 1), new = TRUE)
#' @importFrom checkmate assertClass assertNumber assertNumeric
#' @importFrom raster as.matrix raster extent extent<- crs crs<-
#' @export

rBinarise <- function(obj, thresh = NULL, match = NULL){
  # check arguments
  assertClass(obj, "RasterLayer")
  assertNumber(thresh, null.ok = TRUE)
  assertNumeric(match, null.ok = TRUE)

  mat <- as.matrix(obj)
  vals <- unique(as.vector(mat))

  if(is.null(thresh)){
    thresh <- max(vals)
  } else{
    if(!min(vals, na.rm = TRUE)<thresh | !thresh<=max(vals, na.rm = TRUE)){
      stop("please provide a value for 'thresh' within the range of the values of 'obj'.")
    }
  }

  # in case 'match' is defined, remove these values from 'values'
  if(!is.null(match)){
    values <- vals[!vals %in% match]
  } else{
    values <- vals
  }

  # select only values that are above 'thresh'
  temp <- morphC(mat = mat, kernel = matrix(thresh, 1, 1), value = values, blend = 4,
                 merge = 3, rotateKernel = FALSE, strictKernel = FALSE)

  # in case there are several values in 'match', the matrix needs to be flattened
  if(length(match) > 1){
    temp <- morphC(mat = temp, kernel = matrix(0, 1, 1), value = vals, blend = 4,
                   merge = 3, rotateKernel = FALSE, strictKernel = FALSE)
  }

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history) == 0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("the values have been binarised")))

  names(out) <- "values_binarised"
  return(out)
}

#' Blend two rasters with each other
#'
#' Blend two rasters simply by adding (and weighing) their values per cell.
#' @template obj
#' @param overlay [\code{RasterLayer(1)} | \code{matrix(1)}]\cr the raster that
#'   should be blended with the primary raster (obj); has to have the same
#'   dimension as the primary raster.
#' @param patches [\code{logical(1)}]\cr should the blend be carried out while
#'   considering foreground patches in the primary raster (\code{TRUE}), or
#'   should the blend be carried out without this consideration (\code{FALSE},
#'   default).
#' @param weight [\code{numeric(1)}]\cr the weight the overlay raster should
#'   have as multiple (or fraction) of the primary raster.
#' @return A \code{RasterLayer} of the same dimension as \code{primary}, in
#'   which an overlay has been blended with the primary raster.
#' @details In case \code{patches = TRUE}, the cells in the overlay are grouped
#'   according to the foreground patches in the primary raster and all cells of
#'   this group are assigned their common average, then the blend is carried
#'   out.
#'
#'   A blend operation currently is defined as 'overlay*weight + obj'. To
#'   aggregate more than two \code{RasterLayers} with arbitrary functions, see
#'   \code{\link{rReduce}}.
#' @family operators to modify a raster
#' @examples
#' # define primary raster ...
#' input <- rtData$continuous
#' patches <- rPatches(rBinarise(input, thresh = 30))
#'
#' # ... and an overlay from a matrix
#' m <- matrix(nrow = 56, ncol = 60, data = 0)
#' m[c(5:25), c(5:50)] <- 10
#' mask <- raster::raster(m, xmn=0, xmx=60, ymn=0, ymx=56, crs=NA)
#'
#' # blend while considering patches in the primary raster
#' blended <- rBlend(patches, overlay = mask, patches = TRUE)
#' visualise(raster::stack(patches, blended))
#'
#' # blend while not considering patches
#' visualise(rBlend(patches, overlay = mask, weight = 10))
#' @importFrom checkmate assertClass assert testClass assertLogical assertNumber
#'   assertTRUE
#' @importFrom raster as.matrix raster extent crs crs<-
#' @export

rBlend <- function(obj, overlay, patches = FALSE, weight = 1){

  # check arguments
  assertClass(obj, "RasterLayer")
  assert(
    isRaster <- testClass(overlay, "RasterLayer"),
    isMatrix <- testClass(overlay, "matrix")
  )
  assertLogical(patches)
  assertNumber(weight)

  mat <- as.matrix(obj)
  vals <- unique(as.vector(mat))
  if(isRaster){
    mat_overlay <- as.matrix(overlay)
  } else{
    mat_overlay <- overlay
  }
  assertTRUE(all(dim(mat)==dim(mat_overlay)))

  if(patches){
    clusterIds <- vals[!is.na(vals)]
    for(i in clusterIds){
      mat_overlay[mat%in%i] <- mean(overlay[mat%in%i], na.rm = TRUE)
    }
  }

  # I keep this here because at some point I might implement '#include <boost/...>' in reduceMatrixC
  # matList <- list(mat, mat_overlay*weight)
  # temp <- reduceMatrixC(matList, f = sum)
  temp <- mat_overlay*weight + mat

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("the raster has been blended")))

  names(out) <- "blended"
  return(out)
}

#' Select cells with values between an upper and lower threshold in a raster
#'
#' @template obj
#' @param range [\code{numeric(2)}]\cr minimum and maximum value between which
#'   the values will be selected.
#' @template background
#' @return A \code{RasterLayer} of the same dimension as \code{obj}, where the
#'   cells with values covered by \code{range} retain their value and other
#'   cells are set to \code{NA}.
#' @family operators to select a subset of cells
#' @examples
#' input <- rtData$continuous
#' visualise(rBounded(input, range = c(40, 80)))
#' @importFrom checkmate assertClass assertNumeric
#' @importFrom raster as.matrix raster extent crs crs<-
#' @export

rBounded <- function(obj, range = NULL, background = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertNumeric(range, len = 2)
  assertIntegerish(background, null.ok = TRUE)
  if(is.null(background)){
    background <- NA
  }

  mat <- as.matrix(obj)
  vals <- unique(as.vector(mat))
  lower <- min(range, na.rm = TRUE)
  upper <- max(range, na.rm = TRUE)

  if(!min(vals)<lower | !lower<max(vals) | !min(vals)<upper | !upper<max(vals)){
    stop("please provide values for 'lower' and 'upper' within the range of the values of 'obj'.")
  }
  values <- vals[!duplicated(vals)]

  temp <- morphC(mat = mat, kernel = matrix(lower, 1, 1), value = values, blend = 4,
                 merge = 12, rotateKernel = FALSE, strictKernel = FALSE)
  temp <- morphC(mat = temp, kernel = matrix(upper, 1, 1), value = values, blend = 3,
                 merge = 12, rotateKernel = FALSE, strictKernel = FALSE)

  # set na
  temp[is.na(temp)] <- background

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("values greater than ", lower, " and less than ", upper, " have been selected")))

  names(out) <- paste0("between_", lower, "_and_", upper)
  return(out)
}

#' Assign categories to the values in a raster
#'
#' @template obj
#' @param breaks [\code{integerish(.)}]\cr the values, where categories should
#'   be delimited.
#' @param n [\code{integerish(1)}]\cr number of categories.
#' @return A \code{RasterLayer} of the same dimension as \code{obj}, in which
#'   the cells have the category number into which their values fall.
#' @details Using \code{n} will determine \code{breaks} based on the value-range
#'   of \code{obj} so that the values are assigned to n categories.
#'
#'   Assigning \code{breaks} is mostly usefull when values are to be non-linear,
#'   such as \code{log(min:max)*max/log(max)}, but could also be \code{seq(min,
#'   max, length.out = 21)}, which corresponds to \code{n = 20}.
#' @family operators to modify cell values
#' @examples
#' input <- rtData$continuous
#' visualise(rCategorise(input, n = 5))
#'
#' # use as algorithm in modify, to combine two iterations in one run
#' algorithm <- list(list(operator = "rCategorise", breaks = c(25, 50, 75, 90)),
#'                   list(operator = "rCategorise", breaks = log(1:5)*5/log(5)*20))
#'
#' obj_mod <- modify(input, by = algorithm, merge = TRUE)
#' visualise(obj_mod)
#' @importFrom checkmate assertClass assertNumeric assertNumber
#' @importFrom raster as.matrix values<- values crs crs<-
#' @export

rCategorise <- function(obj, breaks = NULL, n = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertNumeric(breaks, null.ok = TRUE)
  assertIntegerish(n, null.ok = TRUE)

  out <- obj
  mat <- as.matrix(obj)
  vals <- unique(as.vector(mat))
  theRange <- range(vals, na.rm = T)
  if(is.null(breaks)){
    breaks <- seq(theRange[1], theRange[2], length.out = n+1)
  }

  # manage the tails
  if(!any(breaks==theRange[1])){
    breaks <- c(theRange[1], breaks)
  }
  if(!any(breaks==theRange[2])){
    breaks <- c(breaks, theRange[2])
  }
  values(out) <- findInterval(values(out), breaks, rightmost.closed = TRUE)


  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("categories based on the breaks ", paste0(breaks, collapse = "/"), " have been defined")))

  names(out) <- paste0("new_breaks")
  return(out)

}

#' Determine the centroid of patches in a raster
#'
#' The centroid is the average location of all cells of a foreground patch.
#' @template obj
#' @param output [\code{character(1)}]\cr the type in which the output should be
#'   returned. Either \code{"raster"} (default) or \code{"geom"}.
#' @template background
#' @return Depending on \code{output}, either \enumerate{ \item a
#'   \code{RasterLayer} of the same dimension as \code{obj}, in which the
#'   centroid of each foreground patch has the value of the patch-number
#'   identified with \code{\link{rPatches}} and where the remaining cells of
#'   each foreground patch have the value NA or \item a geometry object
#'   of the centroids.}
#' @family operators to determine objects
#' @examples
#' input <- rtData$continuous
#' patches <- rPatches(rBinarise(input, thresh = 30))
#' visualise(rCentroid(patches))
#'
#' # get centroid coordinates
#' centroid_coords <- rCentroid(patches, output = "geom")
#' visualise(gridded = input, geom = centroid_coords, col = "red")
#' @importFrom checkmate assertClass
#' @importFrom raster values rasterToPoints rasterize crs
#' @export

rCentroid <- function(obj, output = "raster", background = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  output <- match.arg(output, c("raster", "geom"))
  assertIntegerish(background, null.ok = TRUE)
  if(is.null(background)){
    background <- NA
  }

  mat <- as.matrix(obj)

  if(isBinaryC(mat)){
    mat <- components(mat, shapeKernel(c(3, 3), type = "diamond"))
  }

  containsNA <- ifelse(anyNA(mat), TRUE, FALSE)
  if(containsNA){
    mat[is.na(mat)] <- 0
  }
  vals <- unique(as.vector(mat))

  # determine centroids by averaging all cell coordinates per patch
  thePoints <- cellToPointsC(mat)
  theMeans <- NULL
  for(i in vals){
    temp <- thePoints[thePoints$value == i,]
    theMeans <- rbind(theMeans, round(colMeans(temp)/0.5)*0.5)
  }
  theMeans <- as.data.frame(theMeans)
  theMeans <- theMeans[order(theMeans$value),]
  if(containsNA){
    theMeans <- theMeans[-which(theMeans$value == 0),]
  }

  if(output == "geom"){
    xt <- extent(obj)
    out <- list(coords = cbind(theMeans[c(1, 2)], id = theMeans[,3]),
                extent = data.frame(x = c(xt@xmin, xt@xmax), y = c(xt@ymin, xt@ymax)),
                type = "point")
    return(out)
  } else{
    out <- rasterize(x = theMeans[c(1, 2)], y = obj, background = background)

    if(length(obj@history)==0){
      history <- list(paste0("the object was loaded from memory"))
    } else{
      history <- obj@history
    }
    out@history <- c(history, list(paste0("the centroids of patches have been determined")))

    names(out) <- "centroids"
    return(out)
  }
}

#' Morphologically dilate foreground patches in a raster
#'
#' The morphological operation 'dilate' adds cells at the boundary of a patch in
#' a binarised raster.
#' @template obj
#' @template kernel1
#' @return A \code{RasterLayer} of the same dimension as \code{obj}, where the
#'   boundaries of the binary raster have been enlarged by the given kernel.
#' @family operators to morphologically modify a raster
#' @examples
#' input <- rtData$continuous
#' binarised <- rBinarise(input, thresh = 30)
#' visualise(rDilate(binarised))
#'
#' # use another kernel
#' (myKernel <- matrix(1, 3, 3))
#' visualise(rDilate(binarised, kernel = myKernel), new = TRUE)
#' @importFrom checkmate assertClass assertMatrix
#' @importFrom raster as.matrix raster extent crs crs<-
#' @export

rDilate <- function(obj, kernel = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertMatrix(kernel, mode = "integerish", all.missing = FALSE, null.ok = TRUE)
  if(is.null(kernel)) kernel <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3)

  mat <- as.matrix(obj)
  blend <- 1 # morphC::blendIdentity
  values <- c(0)
  if(!isBinaryC(mat)){
    values <- NULL
    if(!isBinaryC(kernel)){
      blend <- 5 # morphC::blendPlus
    }
  }

  kernel[kernel==0] <- NA
  temp <- morphC(mat = mat, kernel = kernel, value = values, blend = blend,
                 merge = 2, rotateKernel = FALSE, strictKernel = FALSE)

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("the raster has been morphologically dilated")))

  names(out) <- paste0("dilated")
  return(out)
}

#' Calculate the distance map for a raster
#'
#' The distance map of a binarised raster contains the distance of each
#' background cell to the nearest foreground cell.
#' @template obj
#' @param method [\code{character(1)}]\cr the distance measure to be calculated.
#'   Either \code{"euclidean"} (default), \code{"manhatten"} or
#'   \code{"chessboard"} distance.
#' @return A \code{RasterLayer} of the same dimension as \code{obj}, where the
#'   value of the background cells has been replaced with the distance to the
#'   nearest foreground cell.
#' @details In contrast to \code{\link[raster]{distance}}, the distance
#'   values here do not warp around the boundaries of the map.
#'
#' @references Meijster, A., Roerdink, J.B.T.M., Hesselink, W.H., 2000. A
#'   general algorithm for computing distance transforms in linear time, in:
#'   Goutsias, J., Vincent, L., Bloomberg, D.S. (Eds.), Mathematical Morphology
#'   and Its Applications to Image and Signal Processing. Springer, pp. 331–340.
#' @family operators to modify cell values
#' @examples
#' input <- rtData$continuous
#'
#' # the different distance metrics
#' binarised <- rBinarise(input, thresh = 40)
#' disEuc <- rDistance(binarised)
#' disMan <- rDistance(binarised, method = "manhattan")
#' disChb <- rDistance(binarised, method = "chessboard")
#'
#' distances <- raster::stack(binarised, disEuc, disMan, disChb)
#' visualise(distances)
#'
#' # calculate distance from edge to patch interior
#' inverted <- rPermute(binarised)
#' visualise(rDistance(inverted))
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom raster as.matrix raster extent crs crs<-
#' @export

rDistance <- function(obj, method = "euclidean"){

  # check arguments
  assertClass(obj, "RasterLayer")
  method <- match.arg(method, c("euclidean", "manhattan", "chessboard"))

  mat <- as.matrix(obj)
  if(!isBinaryC(mat)){
    stop("spatial object is not binary, please provide a binarised RasterLayer.")
  }

  temp <- meijsterDistanceC(mat, method = method)

  if(method=="euclidean"){
    temp <- sqrt(temp)
  }

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("distance values have been calculated according to the ", method, " distance")))

  names(out) <- paste0(method, "_distance")

  # manage the bibliography entry
  bib <- bibentry(bibtype = "incollection",
                  title = "A general algorithm for computing distance transforms in linear time",
                  volume = "18",
                  isbn = "978-0-306-47025-7",
                  booktitle = "Mathematical Morphology and its Applications to Image and Signal Processing",
                  publisher = "Springer",
                  author = c(
                    person("A", "Meijster"),
                    person(c("J", "B", "T", "M"), "Roerdink"),
                    person(c("W", "H"), "Hesselink")
                  ),
                  editor = c(
                    person("John", "Goutsias"),
                    person("Luc", "Vincent"),
                    person(c("Dan", "S"), "Bloomberg")
                  ),
                  year = "2000",
                  pages = "331--340"
  )

  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  return(out)
}

#' Morphologically erode foreground patches in a raster
#'
#' The morphological operation 'erode' removes cells at the boundary of a
#' foreground patch in a binarised raster.
#' @template obj
#' @template kernel1
#' @return A \code{RasterLayer} of the same dimension as \code{obj}, where the
#'   boundaries of the binary raster have been eroded away by the given kernel.
#' @family operators to morphologically modify a raster
#' @examples
#' # use as standalone
#' input <- rtData$continuous
#' binarised <- rBinarise(input, thresh = 30)
#' visualise(rErode(obj = binarised))
#'
#' # use another kernel
#' (myKernel <- matrix(1, 3, 3))
#' visualise(rErode(binarised, kernel = myKernel), new = TRUE)
#' @importFrom checkmate assertClass assertMatrix
#' @importFrom raster as.matrix raster extent crs crs<-
#' @export

rErode <- function(obj, kernel = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertMatrix(kernel, nrows = 3, ncols = 3, null.ok = TRUE)
  if(is.null(kernel)) kernel <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3)

  mat <- as.matrix(obj)
  blend <- 1 # morphC::blendIdentity
  values <- c(1)
  if(!isBinaryC(mat)){
    vals <- values(obj)
    values <- vals[!duplicated(vals) & vals!=0]
    if(!isBinaryC(kernel)){
      blend <- 6 # morphC::blendMinus
      values <- NULL
    }
  }

  kernel[kernel==0] <- NA
  temp <- morphC(mat = mat, kernel = kernel, value = values, blend = blend,
                 merge = 1, rotateKernel = FALSE, strictKernel = FALSE)

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)


  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("the raster has been morphologically eroded")))

  names(out) <- paste0("eroded")
  return(out)
}

#' Fill NA values in a raster
#'
#' Assign a value to cells with NA
#' @template obj
#' @param with [\code{integerish(1)}]\cr value with which NA should be replaced.
#' @return a \code{RasterLayer} of the same dimension as \code{obj}, where value
#'   \code{NA} has been replaced by value \code{with}.
#' @family operators to modify cell values
#' @examples
#' input <- rtData$continuous
#' patches <- rPatches(rBinarise(input, thresh = 30))
#'
#' visualise(raster::stack(patches, rFillNA(patches)))
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom raster as.matrix raster extent crs crs<-
#' @export

rFillNA <- function(obj, with = 0){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertIntegerish(with)

  mat <- as.matrix(obj)
  mat[is.na(mat)] <- with

  out <- raster(mat)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("NA has been replaced with ", with)))

  names(out) <- paste0("NA_replaced")
  return(out)
}

#' Select cells with values above a threshold in a raster
#'
#' @template obj
#' @param thresh [\code{numeric(1)}]\cr values above this threshold should be
#'   retained.
#' @template background
#' @return A \code{RasterLayer} of the same dimensions as \code{obj}, in which
#'   all values lower than \code{thresh} have been set to \code{background}.
#' @family operators to select a subset of cells
#' @examples
#' input <- rtData$continuous
#' visualise(rGreater(input, thresh = 40))
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom raster as.matrix values raster extent crs crs<-
#' @importFrom methods as
#' @export

rGreater <- function(obj, thresh, background = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertNumeric(thresh)
  assertIntegerish(background, null.ok = TRUE)
  if(is.null(background)){
    background <- NA
  }

  mat <- as.matrix(obj)
  vals <- unique(as.vector(mat))

  if(!min(vals, na.rm = TRUE)<=thresh | !thresh<=max(vals, na.rm = TRUE)){
    stop("please provide a value for 'thresh' within the range of the values of 'obj'.")
  }

  temp <- morphC(mat = mat, kernel = matrix(thresh, 1, 1), value = vals, blend = 4,
                 merge = 12, rotateKernel = FALSE, strictKernel = FALSE)

  # set na
  temp[is.na(temp)] <- background

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("values greater than ", thresh, " have been selected")))

  names(out) <- paste0("greater_than_", thresh)
  return(out)

}

#' Select cells with values below a threshold in a raster
#'
#' @template obj
#' @param thresh [\code{numeric(1)}]\cr values below this threshold should be
#'   retained.
#' @template background
#' @return A \code{RasterLayer} of the same dimensions as \code{obj}, in which
#'   all values greater than \code{thresh} have been set to \code{NA}.
#' @family operators to select a subset of cells
#' @examples
#' input <- rtData$continuous
#' visualise(rLess(input, thresh = 80))
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom raster as.matrix values raster extent crs crs<-
#' @export

rLess <- function(obj, thresh, background = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertNumeric(thresh)
  assertIntegerish(background, null.ok = TRUE)
  if(is.null(background)){
    background <- NA
  }

  mat <- as.matrix(obj)
  vals <- unique(as.vector(mat))

  if(!min(vals, na.rm = TRUE)<=thresh | !thresh<=max(vals, na.rm = TRUE)){
    stop("please provide a value for 'thresh' within the range of the values of 'obj'.")
  }

  temp <- morphC(mat = mat, kernel = matrix(thresh, 1, 1), value = vals, blend = 3,
                 merge = 12, rotateKernel = FALSE, strictKernel = FALSE)

  # set na
  temp[is.na(temp)] <- background

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("values less than ", thresh, " have been selected")))

  names(out) <- paste0("less_than_", thresh)
  return(out)

}

#' Select cells of a raster based on a mask
#'
#' @template obj
#' @param mask [\code{RasterLayer(1)} | \code{matrix(1)}]\cr Binary object of
#'   the same dimension as \code{obj} where the cells that should be retained
#'   have the value 1 and all other cells the value 0.
#' @template background
#' @return A \code{RasterLayer} of the same dimensions as \code{obj}, in which
#'   all cells with value 0 in the mask have been set to \code{NA} and all other
#'   values are retained.
#' @details If used in an algorithm, \code{mask} can also contain the name of a
#'   sub-algorithm to use the final output thereof as mask. Moreover, \code{mask
#'   = "input"} would select the original raster as mask.
#' @family operators to select a subset of cells
#' @examples
#' input <- rtData$continuous
#' m <- matrix(nrow = 56, ncol = 60, data = 0)
#' m[c(5:25), c(5:50)] <- 1
#'
#' visualise(rMask(input, mask = m))
#'
#' \dontrun{
#'
#' # determine mask interactively
#' mask <- geomPolygon(template = input, vertices = 5, show = T, col = "deeppink")
#' mask <- gToRaster(mask)
#'
#' visualise(gridded = rMask(obj = input, mask = mask))
#' }
#' @importFrom checkmate assertClass
#' @importFrom raster as.matrix raster extent crs crs<-
#' @export

rMask <- function(obj, mask = NULL, background = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  isRaster <- testClass(mask, "RasterLayer")
  isMatrix <- testClass(mask, "matrix")
  if(!isRaster & !isMatrix){
    stop("please provide a mask of the supported type.")
  }
  assertIntegerish(background, null.ok = TRUE)
  if(is.null(background)){
    background <- NA
  }

  if(isRaster){
    mask <- as.matrix(mask)
  }
  if(!isBinaryC(mask)){
    stop("please provide a binary mask.")
  }

  temp <- as.matrix(obj)
  ext <- extent(obj)
  target_crs <- crs(obj)

  if(!all(dim(temp)==dim(mask))){
    stop("please provide a mask of the same dimension as 'obj'.")
  }

  temp[mask==0] <- background
  out <- raster(temp,
                xmn = ext[1], xmx = ext[2], ymn = ext[3], ymx = ext[4],
                crs = target_crs)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("the raster has been masked")))

  names(out) <- "masked"

  return(out)

}

#' Match cells of a raster with a kernel
#'
#' @template obj
#' @template kernel2
#' @param rotate [\code{logical(1)}]\cr should the kernel be applied for all
#'   possible rotations (\code{TRUE}, default) or should the kernel be used
#'   as is (\code{FALSE})?
#' @template background
#' @return A \code{RasterLayer} or \code{RasterStack} of the same dimension as
#'   \code{obj} in which all cells that match with the kernel(s) have the kernel
#'   value and all other cells have the value \code{background}.
#' @details This is also known as the 'hit-or-miss'-transform.
#' @family operators to select a subset of cells
#' @examples
#' input <- rtData$continuous
#' binarised <- rBinarise(input, thresh = 30)
#'
#' # match isolated cells
#' (kIso <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3))
#' matched <- rMatch(binarised, kernel = kIso)
#' visualise(matched)
#'
#' # match all right angled corners
#' (kCorner <- matrix(c(NA, 1, NA, 0, 1, 1, 0, 0, NA), 3, 3))
#' matched <- rMatch(binarised, kernel = kCorner, background = 0)
#' visualise(matched, new = TRUE)
#'
#' # match north-east facing right angled corners and isolated cells
#' matched <- rMatch(binarised, kernel = list(kIso, kCorner), rotate = FALSE)
#' visualise(matched, new = TRUE)
#'
#' # match endpoints of a skeleton
#' skeletonised <- rSkeletonise(binarised, background = 0)
#' (kEnd <- matrix(c(NA, 0, 0, NA, 1, 0, NA, 0, 0), 3, 3))
#' endpoints <- rMatch(skeletonised, kernel = kEnd, background = 0)
#'
#' # match triple points (conjunctions) of a skeleton
#' kConj <- list(matrix(c(NA, 0, 1, 1, 1, NA, NA, 0, 1), 3, 3),
#'               matrix(c(1, NA, 1, NA, 1, NA, NA, NA, 1), 3, 3),
#'               matrix(c(NA, 1, NA, 0, 1, 1, 1, 0, NA), 3, 3))
#' conjunctions <- rMatch(skeletonised, kernel = kConj, background = 0)
#' out <- raster::stack(
#'   rBlend(skeletonised, overlay = endpoints),
#'   rBlend(skeletonised, overlay = conjunctions[[1]]),
#'   rBlend(skeletonised, overlay = conjunctions[[2]]),
#'   rBlend(skeletonised, overlay = conjunctions[[3]]))
#' names(out) <- c("endpoints", "conj1", "conj2", "conj3")
#' visualise(out)
#' @importFrom checkmate assertClass testClass testList assertList assertMatrix
#'   assertIntegerish
#' @importFrom raster as.matrix values raster extent stack crs crs<-
#' @export

rMatch <- function(obj, kernel = NULL, rotate = TRUE, background = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertNumeric(obj@data@values, any.missing = FALSE)
  assert
  if(is.null(kernel)){
    kernel <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3)
  } else{
    isMatrix <- testClass(kernel, "matrix")
    isList <- testList(kernel)
  }
  if(isList){
    assertList(kernel, types = "matrix")
  } else{
    assertMatrix(kernel, null.ok = TRUE)
  }
  assertIntegerish(background, null.ok = TRUE)
  if(is.null(background)){
    background <- NA
  }

  mat <- as.matrix(obj)
  vals <- unique(as.vector(mat))

  HitOrMiss <- function(mat, kernel, values, rotate){
    morphC(mat = mat, kernel = kernel, value = vals, blend = 2,
           merge = 12, rotateKernel = rotate, strictKernel = TRUE)
  }

  if(isList){
    temp <- lapply(seq_along(kernel), function(i){
      temp2 <- HitOrMiss(mat, kernel[[i]], vals, rotate)
      temp2[is.na(temp2)] <- background
      temp2 <- raster(temp2)
      names(temp2) <- paste0("matched_kernel_", i)
      return(temp2)
    })
    out <- stack(temp)
  } else{
    temp <- HitOrMiss(mat = mat, kernel = kernel, values = vals, rotate = rotate)
    temp[is.na(temp)] <- background
    out <- raster(temp)
    names(out) <- "matched_kernel"
  }

  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("cells have been matched with a ", dim(kernel)[1], "x", dim(kernel)[2], " kernel with values ", paste0(as.vector(kernel), collapse = " "))))

  return(out)
}

#' Offset the values in a raster.
#'
#' @template obj
#' @param value [\code{integerish(1)}]\cr the value by which to offset all
#'   values in the raster (can also be negative).
#' @return A \code{RasterLayer} of the same dimension as \code{obj}, in which
#'   all values have been offsetted by \code{value}.
#' @family operators to modify cell values
#' @examples
#' input <- rtData$continuous
#' binarised <- rBinarise(input, thresh = 30)
#' visualise(rOffset(binarised, value = 2))
#' @importFrom checkmate assertClass assertIntegerish
#' @importFrom raster as.matrix extent crs crs<-
#' @export

rOffset <- function(obj, value = 1){

  assertClass(obj, "RasterLayer")
  assertIntegerish(value, any.missing = FALSE, len = 1)

  mat <- as.matrix(obj)

  temp <- mat + value

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, paste0("the raster values have been offset by ", value, "."))

  names(out) <- paste0("offset_by_", value)
  return(out)
}

#' Determine foreground patches in a raster
#'
#' (Foreground) Patches are sets of cells which are connected either directly or
#' via other cells in a binarised raster and which should hence be treated as
#' distinct objects.
#' @template obj
#' @template kernel1
#' @template background
#' @return A \code{RasterLayer} of the same dimension as \code{obj}, in which
#'   neighbouring cells of the foreground have been assigned the same value,
#'   forming patches.
#' @family operators to determine objects
#' @examples
#' input <- rtData$continuous
#' binarised <- rBinarise(input, thresh = 30)
#' visualise(rPatches(binarised))
#' @importFrom checkmate assertClass assertMatrix assertIntegerish
#' @importFrom mmand components
#' @importFrom raster as.matrix raster extent crs crs<-
#' @export

rPatches <- function(obj, kernel = NULL, background = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertMatrix(kernel, mode = "integerish", all.missing = FALSE, null.ok = TRUE)
  if(is.null(kernel)) kernel <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3)
  assertIntegerish(background, null.ok = TRUE)
  if(is.null(background)){
    background <- NA
  }

  mat <- as.matrix(obj)
  if(!isBinaryC(mat)){
    stop("spatial object is not binary, run 'rBinarise()' first.")
  }
  temp <- components(mat, kernel)

  # set na
  temp[is.na(temp)] <- background

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("patches have been determined")))

  names(out) <- "patches"

  # manage the bibliography entry (mmand and/or the algorithm)
  bib <- bibentry(bibtype = "Manual",
                  title = "mmand: Mathematical Morphology in Any Number of Dimensions",
                  author = person("Jon", "Clayden"),
                  year = "2017",
                  note = "R package version 1.5.3",
                  url = "https://CRAN.R-project.org/package=mmand"
  )

  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  return(out)

}

#' Apply a permutation to the cell values of a raster
#'
#' The permutation of a set of cell values leads to a systematic (re)arrangement
#' of all the members of the set.
#' @template obj
#' @param type [\code{character(1)}]\cr the type of permutation that should be
#'   applied. Either \code{"invert"}, \code{"revert"}, \code{"descending"},
#'   \code{"ascending"} or \code{"cycle"}.
#' @param by [\code{integerish(1)}]\cr value by which to apply the permutation;
#'   recently only for \code{type = "cycle"}.
#' @return A \code{RasterLayer} of the same dimensions as \code{obj}, in which
#'   all values have been permuted according to the chosen \code{type}.
#' @family operators to modify cell values
#' @examples
#' input <- rtData$continuous
#'
#' # invert background/foreground
#' visualise(raster::stack(input, rPermute(input)))
#'
#' # sort patch values ascending
#' patches <- rPatches(rBinarise(input, thresh = 30))
#' visualise(raster::stack(patches, rPermute(patches, type = "ascending")))
#'
#' # cycle values backwards by 30
#' visualise(raster::stack(input, rPermute(input, type = "cycle", by = -30)))
#' @importFrom checkmate assertClass
#' @importFrom raster values as.matrix crs crs<-
#' @export

rPermute <- function(obj, type = "invert", by = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  type <- match.arg(type, c("invert", "revert", "descending", "ascending", "cycle"))
  if(type == "cycle"){
    assertIntegerish(by)
  }

  mat <- as.matrix(obj)
  vals <- unique(as.vector(mat))
  vals <- vals[!is.na(vals)]

  if(type == "invert"){
    newVals <- max(vals, na.rm = TRUE) - vals
    action <- "invert"
  } else if(type == "revert"){
    newVals <- rev(vals)
    action <- "revert"
  } else if(type == "descending"){
    newVals <- sort(vals, decreasing = TRUE)
    action <- "sorted into descending order"
  } else if(type == "ascending"){
    newVals <- sort(vals, decreasing = FALSE)
    action <- "sorted into ascending order"
  } else if(type == "cycle"){
    newVals <- vals + by
    newVals[newVals > max(vals, na.rm = TRUE)] <- newVals[newVals > max(vals, na.rm = TRUE)] - max(vals, na.rm = TRUE)
    newVals[newVals < min(vals, na.rm = TRUE)] <- newVals[newVals < min(vals, na.rm = TRUE)] + max(vals, na.rm = TRUE)
    action <- paste0("cycled by ", by)
  }
  temp <- subNumNumC(mat = mat, replace = vals, with = newVals)

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("values have been ", action)))

  names(out) <- "values_permuted"
  return(out)

}

#' Rescale the range of values of a raster
#'
#' @template obj
#' @param range [\code{integer(2)}]\cr vector of minimum and maximum value to
#'   which the values will be scaled.
#' @return A \code{RasterLayer} of the same dimension as \code{obj}, where the
#'   cell values have been rescaled.
#' @family operators to modify cell values
#' @examples
#' input <- rtData$continuous
#' scaled <- rRange(input, range = c(0, 25))
#' visualise(raster::stack(input, scaled))
#' @importFrom checkmate assertClass
#' @importFrom raster extent<- crs crs<-
#' @export

rRange <- function(obj, range = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertIntegerish(range, len = 2)

  mat <- as.matrix(obj)

  # scale to range
  temp <- (mat - min(mat, na.rm = TRUE)) * (range[2] - range[1]) / (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) + range[1]

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("the values have been scaled between ", range[1], " and ", range[2])))

  names(out) <- "scaled"

  return(out)
}

#' Combine a raster stack after segregation
#'
#' Successively combine the layers of a raster stack with each other (similar to
#' \code{\link{Reduce}}).
#' @template obj
#' @param by [\code{list(.)}]\cr the cell values by which the layers shall be
#'   aggregated; see Details.
#' @param fun [\code{function(1)}]\cr find the resulting value of the combined
#'   layers.
#' @param weights [\code{numeric(length(obj))}]\cr weight by which the values in
#'   each layer are multiplied before applying \code{fun}.
#' @param direction [\code{character(1)}]\cr the direction into which the
#'   \code{RasterLayer}s of \code{obj} are supposed to be combine. Either
#'   \code{"right"} (default) or \code{"left"}; see Details.
#' @details The argument \code{direction} takes the direction into which the
#'   layers should be combined. \code{"right"} means that layers are combined
#'   from left to right. \code{rReduce} is based on the functional
#'   \code{\link{Reduce}}, where this wording is handled differently.
#'
#'   The number of layers in the aggregated raster depends on the length of the
#'   list in \code{by}. If by is left empty, everything is written into one
#'   \code{RasterLayer} object. Values to be aggregated in a \code{RasterLayer}
#'   are in the same list element.
#' @return a \code{RasterLayer} or \code{RasterStack} of the same dimensions as
#'   \code{obj}, in which the layers of \code{obj} are aggregated into a smaller
#'   set of layers.
#' @family operators to modify a raster
#' @examples
#' input <- rtData$continuous
#' patches <- rPatches(rBinarise(input, thresh = 30))
#' myPatches <- rSegregate(patches)
#'
#' # revert the segregation
#' visualise(rReduce(myPatches))
#'
#' # group patches
#' by <- list(c(1:14), c(15:28))
#' visualise(rReduce(myPatches, by = by))
#'
#' # select a subset of patches
#' by <- list(c(1, 3, 5, 7, 9))
#' visualise(rReduce(myPatches, by = by))
#' @importFrom checkmate assertClass assertList assertFunction assertIntegerish
#' @importFrom raster as.matrix addLayer overlay brick extent<- crs crs<-
#' @export

rReduce <- function(obj, by = NULL, fun = function(x) sum(x, na.rm = TRUE),
                    weights = NULL, direction = "right"){

  # check arguments
  assertClass(obj, "RasterStackBrick")
  assertList(by, types = "integerish", min.len = 1, null.ok = TRUE)
  assertFunction(fun)
  assertIntegerish(weights, null.ok = TRUE)
  direction <- match.arg(direction, c("right", "left"))

  if(is.null(weights)){
    weights <- rep_len(1, dim(obj)[3])
  } else{
    if(length(weights) != dim(obj)[3]){
      weights <- rep_len(weights, dim(obj)[3])
    }
  }

  if(direction == "right"){
    right <- FALSE
  } else{
    right <- TRUE
  }

  if(!is.null(by)){
    out <- brick()
    for(i in seq_along(by)){

      tempObj <- obj[[by[[i]]]]
      tempWeights <- weights[by[[i]]]
      theRasters <- lapply(X = 1:dim(tempObj)[3], function(x){
        tempObj[[x]] * tempWeights[x]
      })
      tempOut <- Reduce(f = function(x, y) overlay(x, y, fun = fun),
                        x = theRasters,
                        right = right)
      out <- addLayer(out, tempOut)
    }
    action <- paste0(length(by), " new layers")
    names(out) <- paste0("reduced_", seq_along(by))

  } else{

    theRasters <- lapply(X = 1:dim(obj)[3], function(x){
      obj[[x]] * weights[x]
    })
    out <- Reduce(f = function(x, y) overlay(x, y, fun = fun),
                  x = theRasters,
                  right = right)

    # I keep this here because at some point I might implement '#include <boost/...>' in reduceMatrixC
    # theMatrixes <- lapply(1:dim(obj)[3], function(x) as.matrix(obj[[x]]))
    # out <- reduceMatrixC(theMatrixes, f = fun)

    action <- paste0("1 new layer")
    names(out) <- "reduced"
  }

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }

  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  out@history <- c(history, list(paste0("layers have been reduced into ", action)))
  return(out)
}

#' Change the resolution of a raster
#'
#' Rescaling a raster changes the raster's resolution.
#' @template obj
#' @param factor [\code{integer(1)} | \code{numeric(1)}]\cr an integer for
#'   up-scaling and a fraction for down-scaling (see Details).
#' @param kernelFunction [\code{kernelFunction}]\cr A kernel function to
#'   determine new values of the rescaled raster, defaulting to
#'   \code{boxKernel()}; see Details.
#' @return A \code{RasterLayer} with a dimension that has been up or down-scaled
#'   from \code{obj}, where the new values have been determined by applying
#'   \code{kernelFunction}.
#' @details The factor is a value by which the number of cells in x and y
#'   dimension will be multiplied. If the raster should be up-scaled
#'   (\code{factor > 1}), only integer values are allowed (will be truncated if
#'   it is not an integer). If the raster should be down-scaled (\code{factor <
#'   1}) any inverse of an integer value (1/n) is allowed (denominator will be
#'   rounded to the next integer, such as 1/2 or 1/5).
#'
#'   The \code{kernelFunction} used here deviates from the other
#'   (morpholological) kernels in that it is a function, which interpolates the
#'   values of new cells. See \code{\link[mmand]{kernelFunction}} for an
#'   in-depth description.
#' @family operators to modify a raster
#' @examples
#' input <- rtData$continuous
#' visualise(input)
#' visualise(rRescale(input, factor = 0.5))
#'
#' # up-scaling can be useful for follow-up morphological operations
#' binarised <- rBinarise(input, thresh = 30)
#' visualise(rSkeletonise(binarised))
#'
#' rescaled <- rRescale(binarised, factor = 2)
#' visualise(rSkeletonise(rescaled), new = TRUE)
#' @importFrom checkmate assertClass
#' @importFrom raster as.matrix raster extent extent<- crs crs<-
#' @importFrom mmand boxKernel rescale
#' @export

rRescale <- function(obj, factor = NULL, kernelFunction = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertNumeric(factor, any.missing = FALSE, len = 1)
  if(is.null(kernelFunction)){
    kernelFunction <- boxKernel()
  } else{
    assertClass(kernelFunction, "kernelFunction")
  }

  mat <- as.matrix(obj)

  if(factor > 1){
    factor <- trunc(factor)
  } else{
    denominator <- round(1/factor)
    factor <- 1/denominator
  }
  temp <- rescale(mat, factor, kernelFunction)

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("the raster has been rescaled by the factor ", factor)))

  names(out) <- "rescaled"

  # manage the bibliography entry (mmand)
  bib <- bibentry(bibtype = "Manual",
                  title = "mmand: Mathematical Morphology in Any Number of Dimensions",
                  author = person("Jon", "Clayden"),
                  year = "2017",
                  note = "R package version 1.5.3",
                  url = "https://CRAN.R-project.org/package=mmand"
  )

  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  return(out)
}

#' Segregate values in a raster into layers
#'
#' Distinct values in a raster will be assigned to layers in a raster stack.
#' @template obj
#' @param by [\code{RasterLayer(1)} | \code{matrix(1)}]\cr additional object by
#'   which \code{obj} should be segregated. If left empty, the distinct values
#'   of \code{obj} will be taken.
#' @param flatten [\code{logical(1)}]\cr should all values be set to value 1
#'   (\code{TRUE}) or should the original \code{obj} values be retained
#'   (\code{FALSE}, default)?
#' @template background
#' @return a \code{RasterLayer} stack of the same dimensions as \code{obj}, in
#'   which the elements specified in \code{by} or the distinct values of
#'   \code{obj} have each been assigned to a layer of the raster stack.
#' @family operators to modify a raster
#' @examples
#' input <- rtData$continuous
#' patches <- rPatches(rBinarise(input, thresh = 30), background = 0)
#' myPatches <- rSegregate(patches)
#' visualise(myPatches[[c(2, 3, 12, 16)]])
#'
#' # when flattening, all values are set to 1
#' myPatches2 <- rSegregate(patches, flatten = TRUE)
#' visualise(myPatches2[[c(2, 3, 12, 16)]], new = TRUE)
#'
#' # cut out by 'patches'
#' patchValues <- rSegregate(input, by = patches)
#' visualise(patchValues[[c(2, 3, 12, 16)]], new = TRUE)
#' @importFrom checkmate assertClass testNull assert testClass assertLogical
#'   assertIntegerish
#' @importFrom raster as.matrix values extent<- stack crs crs<-
#' @importFrom mmand shapeKernel components
#' @export

rSegregate <- function(obj, by = NULL, flatten = FALSE, background = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  existsBy <- !testNull(by)
  if(existsBy){
    isRaster <- testClass(by, "RasterLayer")
    isMatrix <- testClass(by, "matrix")
    if(!isRaster & !isMatrix){
      stop("please provide a valid 'by'.")
    }
  }
  assertLogical(flatten)
  assertIntegerish(background, null.ok = TRUE)
  if(is.null(background)){
    background <- NA
  }

  mat <- as.matrix(obj)
  if(!existsBy){
    subMat <- mat
  } else{
    if(isRaster){
      subMat <- as.matrix(by)
    } else{
      subMat <- by
    }
  }
  vals <- unique(as.vector(subMat))
  vals <- vals[!is.na(vals)]

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }

  theRasters <- list()
  for(i in seq_along(vals)){

    tempMat <- mat
    tempMat[subMat != vals[i] | is.na(subMat)] <- background
    if(flatten){
      tempMat[subMat == vals[i]] <- 1
    }

    tempRaster <- raster(tempMat)
    extent(tempRaster) <- extent(obj)
    crs(tempRaster) <- crs(obj)
    tempRaster@history <- c(history, list(paste0("element ", vals[i], " has been segregated from the main raster")))

    theRasters <- c(theRasters, list(tempRaster))

  }
  theRasters <- stack(theRasters)
  theRasters@history <- c(history, list(paste0("the raster has been segregated")))
  names(theRasters) <- paste0("element_", vals)

  # manage the bibliography entry (mmand)
  bib <- bibentry(bibtype = "Manual",
                  title = "mmand: Mathematical Morphology in Any Number of Dimensions",
                  author = person("Jon", "Clayden"),
                  year = "2017",
                  note = "R package version 1.5.3",
                  url = "https://CRAN.R-project.org/package=mmand"
  )

  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  return(theRasters)
}

#' Determine the skeleton of foreground patches in a raster
#'
#' The morphological skeleton of a patch is a binary skeletal remnant that
#' preserves the extent and connectivity (i.e. the topology) of the patch.
#' @template obj
#' @param method [\code{character(1)}]\cr the method to determine the skeleton.
#'   Either \code{"hitormiss"} (default), \code{"lantuejoul"} or
#'   \code{"beucher"}.
#' @template kernel1
#' @template background
#' @return a \code{RasterLayer} of the same dimensions as \code{obj}, in which
#'   foreground patches have been transformed into their morphological
#'   skeletons.
#' @seealso \code{\link{rDistance}}
#' @details For details, refer to \code{\link[mmand]{skeletonise}} of which
#'   \code{rSkeletonise} is a wrapper.
#' @family operators to determine objects
#' @examples
#' input <- rtData$continuous
#' binarised <- rBinarise(input, thresh = 30)
#'
#' visualise(raster::stack(binarised, rSkeletonise(binarised)))
#' @importFrom checkmate assertClass
#' @importFrom mmand shapeKernel skeletonise
#' @importFrom raster as.matrix raster extent extent<- crs crs<-
#' @export

rSkeletonise <- function(obj, method = "hitormiss", kernel = NULL, background = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  method <- match.arg(arg = method, choices = c("lantuejoul", "beucher", "hitormiss"))
  assertMatrix(kernel, mode = "integerish", all.missing = FALSE, null.ok = TRUE)
  if(is.null(kernel)) kernel <- matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0), 3, 3)
  assertIntegerish(background, null.ok = TRUE)
  if(is.null(background)){
    background <- NA
  }

  if(missing(kernel)){
    kernel <- shapeKernel(c(3, 3), type="diamond")
  }

  mat <- as.matrix(obj)
  if(!isBinaryC(mat)){
    stop("spatial object is not binary, please run 'rBinarise()' initially.")
  }
  temp <- skeletonise(mat, kernel, method = method)

  # set na
  temp[temp == 0] <- background

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("the morphological skeleton has been determined")))

  names(out) <- "skeletonised"

  # manage the bibliography entry (mmand)
  bib <- bibentry(bibtype = "Manual",
                  title = "mmand: Mathematical Morphology in Any Number of Dimensions",
                  author = person("Jon", "Clayden"),
                  year = "2017",
                  note = "R package version 1.5.3",
                  url = "https://CRAN.R-project.org/package=mmand"
  )

  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  return(out)
}

#' Substitute values in a raster
#'
#' @template obj
#' @param old [\code{integerish(.)}]\cr values to be substituted.
#' @param new [\code{integerish(.)}]\cr  values to substitute with.
#' @return A \code{RasterLayer} of the same dimensions as \code{obj}, in which
#'   \code{old} values have been replaced by \code{new} values.
#' @family operators to modify cell values
#' @examples
#' input <- rtData$categorical
#' substituted <- rSubstitute(input, old = c(41:47), new = 40)
#' visualise(raster::brick(input, substituted))
#' @importFrom checkmate assertClass
#' @importFrom raster values as.matrix extent<- crs crs<-
#' @export

rSubstitute <- function(obj, old = NULL, new = NULL){

  # check arguments
  assertClass(obj, "RasterLayer")
  assertIntegerish(old, any.missing = FALSE, min.len = 1)
  assertIntegerish(new, any.missing = FALSE, min.len = 1)

  if(length(old)!=length(new)){
    new <- rep(new, length.out = length(old))
  }

  mat <- as.matrix(obj)
  temp <- subNumNumC(mat = mat, replace = old, with = new)

  out <- raster(temp)
  extent(out) <- extent(obj)
  crs(out) <- crs(obj)

  if(length(obj@history)==0){
    history <- list(paste0("the object was loaded from memory"))
  } else{
    history <- obj@history
  }
  out@history <- c(history, list(paste0("value of ", old, " replaced with ", new)))

  names(out) <- "values_substituted"
  return(out)
}
