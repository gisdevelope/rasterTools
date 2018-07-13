#' Adjacency of cells
#'
#' Calculate the cell-adjacency matrix for a raster
#' @template obj2
#' @param type [\code{character(1)}]\cr which type of adjacencies to calculate;
#'   either \code{"like"} adjacencies between cells of the same value,
#'   adjacencies between \code{"paired"} values or the rowsum of the paired
#'   values (\code{"pairedSum"}).
#' @param count [\code{character(1)}]\cr the cells that should be counted;
#'   possible values are \code{"single"} to count only cells to the right and
#'   bottom of the focal cell and \code{"double"} to count additionally cells to
#'   the left and top of the focal cell.
#' @param layer [\code{character(1)}]\cr in case \code{obj} has several layers,
#'   specify here the layer for which the area of objects shall be calculated
#'   (by default, the first layer).
#' @details In case \code{type = "like"}, only the diagonal of the adjacency
#'   matrix is returned, in case \code{type = "paired"}, the complete adjacency
#'   matrix is returned.
#' @return a \code{data.frame} of the frequency of adjacencies of the values in
#'   \code{obj}.
#' @family generic metrics
#' @examples
#' cat <- rtData$categorial
#' bin <- rBinarise(rtData$continuous, thresh = 40)
#'
#' # adjacencies in conjunction with 'measure'
#' mAdj <- list(operator = "mAdjacency")
#' (adjacencies <- measure(input = cat, with = "mAdj"))
#'
#' # paired adjacencies
#' mAdjacency(obj = cat, type = "paired")
#'
#' # adjacencies with single count
#' mAdjacency(obj = bin, count = "single")
#'
#' @importFrom checkmate assertClass assertCharacter
#' @importFrom raster as.matrix
#' @export

mAdjacency <- function(obj, type = "like", count = "double", layer = NULL){

  assertClass(obj, "Raster")
  type <- match.arg(type, c("like", "paired", "pairedSum"))
  count <- match.arg(count, c("single", "double"))
  if(count == "single"){
    countDouble <- FALSE
  } else{
    countDouble <- TRUE
  }
  assertCharacter(layer, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(is.null(layer)){
    layer <- names(obj)[1]
  } else{
    layer <- names(obj)[grep(pattern = layer, x = names(obj))]
  }

  # transform obj into mat and count adjacencies
  mat <- as.matrix(eval(parse(text = paste0("obj$", layer))))
  vals <- sort(base::unique(as.vector(mat[!is.na(mat)])))
  values <- countAdjacenciesC(mat = mat, countDouble = countDouble)
  rownames(values) <- vals
  colnames(values) <- vals

  if(type == "like"){

    result <- data.frame(class = vals, likeAdj = diag(values))

  } else if(type == "paired"){

    values <- as.data.frame(values)
    result <- cbind(class = vals, values)

  } else{

    result <- data.frame(class = vals, pairedSum = rowSums(values))

  }


  return(result)
}

#' Area of objects
#'
#' Calculate the area of objects in a raster
#' @template obj2
#' @param scale [\code{character(1)}]\cr scale at which the area of objects
#'   should be calculated; possible values are \code{"patch"}, \code{"class"}
#'   and \code{"window"}.
#' @param unit [\code{character(1)}]\cr the unit the output should have. With
#'   \code{"map"} the result will be in the respective map unit and with
#'   \code{"cells"} (default) it will be the number of raster cells.
#' @param layer [\code{character(1)}]\cr in case \code{obj} has several layers,
#'   specify here the layer for which the area of objects shall be calculated
#'   (by default, the first layer).
#' @return For \code{scale = "window"} the area of the overall raster, for
#'   \code{scale = "class"} the total area of each unique value (class), for
#'   \code{scale = "patch"} the area of distinct objects per distinct values
#'   (i.e. the area of patches per class).
#' @family generic metrics
#' @examples
#' cat <- rtData$categorial
#' bin <- rBinarise(rtData$continuous, thresh = 40)
#'
#' # the area ...
#' # ... in conjunction with 'measure'
#' mAc <- list(operator = "mArea", scale = "class")
#' (areas <- measure(input = cat, with = "mAc"))
#'
#' # ... per landuse type
#' mArea(obj = cat, scale = "class")
#'
#' # ... of patches per landuse type
#' mArea(obj = cat, scale = "patch")
#'
#' # ...  of certain values
#' require(magrittr)
#' rBinarise(obj = cat, match = c(41, 44, 47)) %>%
#'   mArea(scale = "patch", layer = "values_binarised")
#'
#' # ... from a binary raster
#' mArea(obj = bin, scale = "patch")
#' mArea(obj = bin, scale = "class")
#'
#' @importFrom checkmate assertClass assertCharacter
#' @importFrom raster res as.matrix
#' @importFrom mmand components shapeKernel
#' @export

mArea <- function(obj, scale = "patch", unit = "cells", layer = NULL){

  assertClass(obj, "Raster")
  scale <- match.arg(scale, c("patch", "class", "window"))
  unit <- match.arg(unit, c("cells", "map"))
  assertCharacter(layer, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(is.null(layer)){
    layer <- names(obj)[1]
  } else{
    layer <- names(obj)[grep(pattern = layer, x = names(obj))]
  }

  # transform obj into mat
  mat <- as.matrix(eval(parse(text = paste0("obj$", layer))))
  resolution <- res(obj)

  # do calculation for the respective scale
  if(scale == "window"){

    groups <- data.frame(window = 1)
    values <- data.frame(cells = dim(mat)[1] * dim(mat)[2])

  } else if(scale == "class"){

    temp <- mat
    temp[is.na(temp)] <- 0

    # count...
    values <- countCellsC(mat = temp)
    groups <- data.frame(class = values$value)

  } else {

    # test which object we are dealing with and adapt temp accordingly
    if(isBinaryC(mat)){
      temp <- components(mat, shapeKernel(c(3, 3), type = "diamond"))
    } else{
      temp <- mat
    }
    vals <- sort(base::unique(as.vector(temp[!is.na(temp)])))

    values <- NULL
    groups <- NULL
    for(i in seq_along(vals)){
      temp_mat <- mat
      temp_mat[temp_mat != vals[i]] <- NA
      temp_cc <- components(temp_mat, shapeKernel(c(3, 3), type = "diamond"))

      # count...
      temp_vals <- countCellsC(mat = temp_cc)
      temp_vals <- temp_vals[!is.na(temp_vals$value),]
      values <- rbind(values, temp_vals)
      groups <- c(groups, rep(vals[i], length(temp_vals$value)))
    }
    groups <- cbind(class = groups, patch = values$value)

  }

# put together the output
  if(unit == "map"){
    cells <- values$cells * resolution[1]*resolution[2]
    result <- data.frame(groups, area = cells)
  } else{
    cells <- values$cells
    result <- data.frame(groups, cells)
  }

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

  return(result)
}

#' Number of objects
#'
#' Count the number of objects in a raster
#' @template obj2
#' @param scale [\code{character(1)}]\cr scale at which the area of objects
#'   should be calculated; possible values are \code{"patch"} and \code{"class"}.
#' @param layer [\code{character(1)}]\cr in case \code{obj} has several layers,
#'   specify here the layer for which the area of objects shall be calculated
#'   (by default, the first layer).
#' @return For \code{scale = "class"} the number of unique values (classes) in
#'   the raster. For \code{scale = "patch"} the number of objects per distinc
#'   value (i.e. the number of patches per class).
#' @family generic metrics
#' @examples
#' cat <- rtData$categorial
#' bin <- rBinarise(rtData$continuous, thresh = 40)
#'
#' # the number ...
#' # ... in conjunction with 'measure'
#' mNp <- list(operator = "mNumber", scale = "patch")
#' measure(input = cat, with = "mNp")
#'
#' # ... per landuse type
#' mNumber(obj = cat, scale = "class")
#'
#' # ... of patches per landuse type
#' mNumber(obj = cat, scale = "patch")
#'
#' # ...  of certain values
#' require(magrittr)
#' rBinarise(obj = cat, match = c(41, 44, 47)) %>%
#'   mNumber(scale = "patch", layer = "values_binarised")
#'
#' # ... from a binary raster
#' mNumber(obj = bin, scale = "patch")
#' mNumber(obj = bin, scale = "class")
#'
#' @importFrom checkmate assertClass assertCharacter
#' @importFrom raster as.matrix
#' @importFrom mmand components shapeKernel
#' @export

mNumber <- function(obj, scale = "patch", layer = NULL){

  assertClass(obj, "Raster")
  scale <- match.arg(scale, c("patch", "class"))
  assertCharacter(layer, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(is.null(layer)){
    layer <- names(obj)[1]
  } else{
    layer <- names(obj)[grep(pattern = layer, x = names(obj))]
  }

  # transform obj into mat
  mat <- as.matrix(eval(parse(text = paste0("obj$", layer))))
  resolution <- res(obj)

  # do calculation for the respective scale
  if(scale == "class"){

    vals <- base::unique(as.vector(mat[!is.na(mat)]))
    values <- data.frame(window = 1, classes = length(vals))

  } else{

    # test which object we are dealing with
    if(isBinaryC(mat)){

      temp <- components(mat, shapeKernel(c(3, 3), type = "diamond"))
      vals <- base::unique(as.vector(temp[!is.na(temp)]))
      values <- data.frame(window = 1, patches = length(vals))

    } else {

      vals <- sort(base::unique(as.vector(mat[!is.na(mat)])))

      patches <- NULL
      for(i in seq_along(vals)){
        temp_mat <- mat
        temp_mat[temp_mat != vals[i]] <- NA

        temp_cc <- components(temp_mat, shapeKernel(c(3, 3), type = "diamond"))
        temp_vals <- sort(unique(as.vector(temp_cc)))
        patches <- c(patches, length(temp_vals))
      }
      values <- data.frame(class = vals, patches = patches)

    }

  }

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

  return(values)
}

#' Edge length
#'
#' Calculate the length of edges/boundaries between objects in a spatial raster.
#'
#' @template obj2
#' @param scale [\code{character(1)}]\cr scale at which the area of objects
#'   should be calculated; possible values are \code{"patch"} and
#'   \code{"class"}.
#' @param unit [\code{character(1)}]\cr the unit the output should have. With
#'   \code{"map"} the result will be in the respective map unit and with
#'   \code{"cells"} (default) it will be the multiple of the raster cell
#'   dimension.
#' @param layer [\code{character(1)}]\cr in case \code{obj} has several layers,
#'   specify here the layer for which the area of objects shall be calculated
#'   (by default, the first layer).
#' @return For \code{scale = "class"} the edge length of each unique value
#'   (class) in the raster, for \code{scale = "patch"} the edge length of
#'   distinct objects per distinct values (i.e. the area of patches per class).
#' @family generic metrics
#' @examples
#' cat <- rtData$categorial
#' bin <- rBinarise(rtData$continuous, thresh = 40)
#'
#' # the perimeter ...
#' # ... in conjunction with 'measure'
#' mPc <- list(operator = "mPerimeter", scale = "class")
#' measure(input = cat, with = "mPc")
#'
#' # ... per landuse type
#' mPerimeter(obj = cat, scale = "class")
#'
#' # ... of patches per landuse typ
#' mPerimeter(obj = cat, scale = "patch")
#'
#' # ...  of certain values
#' require(magrittr)
#' rBinarise(obj = cat, match = c(41, 44, 47)) %>%
#'   mPerimeter(scale = "patch", layer = "values_binarised")
#'
#' # ... from a binary raster
#' mPerimeter(obj = bin, scale = "patch")
#' mPerimeter(obj = bin, scale = "class")
#'
#' @importFrom checkmate assertClass assertCharacter
#' @importFrom raster as.matrix values
#' @importFrom mmand components shapeKernel
#' @export

mPerimeter <- function(obj, scale = "patch", unit = "cells", layer = NULL){

  assertClass(obj, "Raster")
  scale <- match.arg(scale, c("patch", "class"))
  unit <- match.arg(unit, c("cells", "map"))
  assertCharacter(layer, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(is.null(layer)){
    layer <- names(obj)[1]
  } else{
    layer <- names(obj)[grep(pattern = layer, x = names(obj))]
  }

  # transform obj into mat
  mat <- as.matrix(eval(parse(text = paste0("obj$", layer))))
  resolution <- res(obj)

  # test which object we are dealing with
  if(scale == "class"){

    temp <- mat
    temp[is.na(temp)] <- 0

    # count...
    values <- countEdgesC(mat = temp)
    groups <- data.frame(class = values$value)

  } else{

    # test which object we are dealing with and adapt temp accordingly
    if(isBinaryC(mat)){
      temp <- components(mat, shapeKernel(c(3, 3), type = "diamond"))
    } else{
      temp <- mat
    }

    vals <- sort(base::unique(as.vector(temp[!is.na(temp)])))

    values <- NULL
    groups <- NULL
    for(i in seq_along(vals)){
      temp_mat <- mat
      temp_mat[temp_mat != vals[i]] <- NA
      temp_cc <- components(temp_mat, shapeKernel(c(3, 3), type = "diamond"))
      temp_cc[is.na(temp_cc)] <- 0

      # count...
      temp_vals <- countEdgesC(mat = temp_cc)
      temp_vals <- temp_vals[temp_vals$value != 0,]
      values <- rbind(values, temp_vals)
      groups <- c(groups, rep(vals[i], length(temp_vals$value)))
    }
    groups <- cbind(class = groups, patch = values$value)

  }

  # put together the output
  if(unit == "map"){
    edges <- values$edgesX * resolution[1] + values$edgesY * resolution[2]
    result <- data.frame(groups, edgelength = edges)
  } else{
    edges <- values$edgesX + values$edgesY
    result <- data.frame(groups, edges)
  }

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

  return(result)
}

#' Cell value distribution
#'
#' Calculate distribution parameters of cell values of a raster
#' @template obj2
#' @param param [\code{character(.)}]\cr parameter(s) to calculate; possible
#'   parameters are \code{"mean"}, \code{"sum"}, \code{"number"}, \code{"sd"},
#'   \code{"cv"}, \code{"iqr"}, \code{"min"}, \code{"median"}, \code{"max"},
#'   \code{"quantile"}, \code{"weighted.mean"}, \code{"identity"}, \code{"all"}
#'   or \code{"raw"}.
#' @param layer [\code{character(1)}]\cr in case \code{obj} has several layers,
#'   specify here the layer for which the area of objects shall be calculated
#'   (by default, the first layer).
#' @param groupBy another layer (typically part of the same raster::brick),
#'   according which you want to divide the cells in the input layer. see
#'   Details.
#' @return A list with elements resulting from the stratification with the value
#'   of each specified parameter per object in \code{groupBby}.
#' @details You can give a raster stack with at least two layers as \code{obj}
#'   and then use \code{groupBby} to 'mask' the cells, so to speak.
#' @family generic metrics
#' @examples
#' cat <- rtData$categorial
#' con <- rtData$continuous
#' bin <- rBinarise(con, thresh = 40)
#' patches <- rPatches(bin)
#'
#' input <- raster::stack(con, patches)
#' @importFrom checkmate assertClass assertCharacter
#' @importFrom stats weighted.mean quantile
#' @export

mValues <- function(obj, param = NULL, layer = NULL, groupBy = NULL){

  # check arguments
  isRaster <- testClass(obj, "Raster")
  isStackBrick <- testClass(obj, "RasterStackBrick")
  if(!isRaster & !isStackBrick){
    stop("please provide a vaid 'obj'.")
  }
  param <- match.arg(param, c("mean", "sum", "number", "sd", "cv", "iqr", "min",
                              "median", "max", "quantile", "weighted.mean", "all",
                              "raw"), several.ok = TRUE)
  assertCharacter(layer, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(is.null(layer)){
    layer <- names(obj)[1]
  } else{
    layer <- names(obj)[grep(pattern = layer, x = names(obj))]
  }
  assertCharacter(groupBy, ignore.case = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  if(isStackBrick){
    if(is.null(groupBy)){
      groupBy <- names(obj)[2]
    } else{
      groupBy <- names(obj)[grep(pattern = groupBy, x = names(obj))]
    }
  }

  mat <- as.matrix(eval(parse(text = paste0("obj$", layer))))

  if(any(param == "all")){
    param <- c("mean", "sum", "number", "sd", "cv", "iqr", "min", "median", "max", "quantile", "weighted.mean")
  }

  # manage parameter names
  if(any(param == "iqr")){
    param[which(param == "iqr")] <- "IQR"
  }
  if(any(param == "number")){
    param[which(param == "number")] <- "length"
  }

  # make list of values and levels to process
  if(!is.null(groupBy)){
    grouped <- as.matrix(eval(parse(text = paste0("obj$", groupBy))))
    vals <- sort(base::unique(as.vector(grouped[!is.na(grouped)])))
    values <- lapply(seq_along(vals), function(x){
      temp <- mat[grouped == vals[x]]
      temp[!is.na(temp)]
    })
    # values <- setNames(values, vals)
  } else{
    values <- list(values(obj))
    vals <- 1
  }

  if(param == "raw"){

    out <- setNames(list(values), "raw")

  } else{

    # handle weighted.mean and raw, because they can't be retrieved with the below do.call snippet
    if(any(param == "weighted.mean")){
      out_wm <- sapply(seq_along(vals), function(x){
        tempVals <- table(values[[x]])
        dimnames(tempVals) <- NULL
        tempWeights <- rep(tempVals, tempVals)
        stats::weighted.mean(x = sort(values[[x]]),
                             w = tempWeights)
      })
      param <- param[-which(param == "weighted.mean")]
    }

    # split quantiles up into list elements
    if(any(param=="quantile")){
      out_q <- sapply(seq_along(values), function(x){
        quantile(values[[x]])
      })
      out_q <- t(out_q)
      colnames(out_q) <- paste0("q", substr(colnames(out_q), 0, nchar(colnames(out_q))-1))
      param <- param[-which(param =="quantile")]
    }

    # handle all other functions
    out <- sapply(seq_along(param), function(i){
      sapply(seq_along(values), function(j){
        do.call(what = param[i], args = list(values[[j]]))
      })
    })
    colnames(out) <- param
    if(!missing(out_wm)) out <- cbind(out, wgh.mean = out_wm)
    if(!missing(out_q)) out <- cbind(out, out_q)

    result <- data.frame(out)

  }

  return(result)
}