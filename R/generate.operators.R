#' Generate a (neutral) gradient pattern
#' 
#' This is largely still work in progress.
#' 
#' A gradient can be understood in terms of the distance to an origin. With a
#' straight line as origin, one might end up with a edge gradient, or, in case
#' the line does not cross the the plot window, a planar gradient. Beware that
#' in case you provide an origin or a strict set of parameters (\code{...}), the
#' spatial pattern may not be neutral anymore!
#' @template mat
#' @param origin [\code{RasterLayer} | \code{matrix}]\cr a binary object that
#'   serves as origin of the gradient; overrides the other arguments, if given.
#' @param type [\code{character(1)}]\cr the type of the gradient model. Either
#'   \code{"planar"} (default), \code{"point"}, \code{"line"}, \code{"polygon"},
#'   special cases thereof or \code{"random"}, to select one by chance.
#' @param ... parameters to create the origin.
#' @details In case \code{origin} is not given, \code{nlmGradient} constructs
#'   internally a binary origin from what is specified in \code{type} and
#'   \code{params} and then provides a distance matrix that has been scaled
#'   between 0 and 1 as gradient. Each geometry requires at least the number of
#'   vertices and their coordinates. In case the required arguments are not
#'   specified in \code{params = list(...)}, they will be set randomly.
#' @examples
#' # create a point gradient based on an origin
#' mat <- matrix(nrow = 100, ncol = 100, data = 0)
#' origin <- mat; origin[5000] <- 1
#' myPointGradient <- spmGradient(mat = mat, origin = origin)
#' visualise(raster = myPointGradient)
#'
#' # create a geometry object
#' #coords <- data.frame(x = c(0.4, 0.45, 0.7, 0.5),
#' #                     y = c(0.4, 0.4, 0.6, 0.7),
#' #                     id = 1)
#' #window <- data.frame(x = c(0, 1),
#' #                     y = c(0, 1))
#' #aGeom <- geomPolygon(anchor = coords, window = window, show = TRUE)
#'
#' # create gradient from the parameters of a geom
#' #NLMPolyGrad <- spmGradient(mat = mat, type = "polygon")
#'
#' # create a gradient from a random point pattern
#' #NLMPointGrad <- spmGradient(mat = mat, type = "point")
#'
#' # create a completely random gradient
#' #RandGrad <- spmGradient(mat = mat, type = "random")
#'
#' #visualise(raster = raster::stack(NLMPolyGrad, NLMPointGrad, RandGrad))
#' @importFrom checkmate assertMatrix testClass assertCharacter assertSubset
#'   assertList
#' @importFrom raster raster as.matrix
#' @export

spmGradient <- function(mat, origin = NULL, type = "planar", ...){

  assertMatrix(mat)
  assertCharacter(type, any.missing = FALSE, len = 1)
  existsOrigin <- !is.null(origin)
  if(existsOrigin){
    isRaster <- testClass(origin, "RasterLayer")
    isMatrix <- testClass(origin, "matrix")
  }
  types <- c("planar", "point", "line", "rectangle", "square", "polygon", "spline", "ellipse", "circle", "triangle", "hexagon")
  if(type == "random"){
    type <- sample(types, 1)
  } else{
    assertSubset(type, choices = types)
  }
  
  theArgs <- listArgs()
  newArgs <- theArgs[!names(theArgs) %in% c("mat", "origin", "type")]
  if(length(newArgs) == 0){
    makeRandom <- TRUE
  } else{
    assertNames(newArgs, subset.of = c("anchor", "vertices"))
    makeRandom <- FALSE
  }
  
  if(!existsOrigin){
    
    if(makeRandom){
      theGeom <- geomRand(type = type, template = mat)
      theMask <- as.matrix(gToRaster(theGeom))
    } else{
      
      if(type == "planar"){
        # planar gradient is a gradient where a linear line through the plot window is layed and moved orthogonally until it touches the plot boundary in only one value        

        
      } else{
        # put together the call from 'type' and 'theArgs' and 'newArgs'

        
      }
      
    }
  } else{
    if(isRaster){
      origin <- as.matrix(origin)
    }
    if(!isBinaryC(origin)){
      stop("please provide a binary raster or matrix in 'origin'.")
    } else{
      theMask <- origin
    }
  }

  # create the distance object, thus the gradient
  temp <- sqrt(meijsterDistanceC(theMask, method = "euclidean"))

  # scale between 0 and 1
  temp <- scaleMat(temp, c(0, 1))

  out <- raster(temp, xmn = 0, xmx = ncol(temp), ymn = 0, ymx = nrow(temp))

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

  names(out) <- paste0(type, "_gradient_nlm")
  return(out)
}

#' Generate a neutral random pattern
#'
#' This is largely still work in progress.
#' 
#' Random values can be applied to different patterns within the landscape
#' model.
#' @template mat
#' @param pattern the pattern to which random values are assigned, either
#'   \code{"cell"}, \code{"rectangle"}, \code{"circle"}, \code{"voronoi"} or
#'   \code{"cluster"}; see Details.
#' @param seed specify a seed for the random creation of numbers.
#' @details \itemize{ \item For \code{pattern = "cell"}, each cell of the resulting
#'   model is assigned a random value. \item For \code{pattern = "rectangle"}, \item
#'   For \code{pattern = "circle"}, \item For \code{pattern = "voronoi"}, Gaucherel
#'   (2008) \item For \code{pattern = "cluster"}, Saura & Martínez-Millán (2000) }
#' @references Gaucherel, C. (2008) Neutral models for polygonal landscapes with
#'   linear networks. Ecological Modelling, 219, 39 - 48. Saura, S.,
#'   Martínez-Millán, J. (2000) Landscape patterns simulation with a modified
#'   random clusters method. Landscape Ecology 15, 661–678.
#' @examples 
#' mat <- matrix(nrow = 100, ncol = 100, data = 0)
#' myRandomPattern <- spmRandom(mat = mat)
#' visualise(raster = myRandomPattern)
#' 
#' @importFrom raster raster
#' @export

spmRandom <- function(mat, pattern = "cell", seed = NULL){

  # https://en.wikipedia.org/wiki/Centroidal_Voronoi_tessellation


  patterns <- c("cell", "rectangle", "circle", "voronoi", "cluster")
  assertSubset(pattern, choices = patterns)
  
  if(!is.null(seed)){
    set.seed(seed)
  }

  if(pattern == "cell"){
    values <- runif(length(mat), min = 0, max = 1)
    mat[] <- values
  } else if(pattern == "rectangle"){

  } else if(pattern == "circle"){

  } else if(pattern == "voronoi"){

  }

  out <- raster(mat, xmn = 0, xmx = ncol(mat), ymn = 0, ymx = nrow(mat))

  # # manage the bibliography entry (the different algos)
  # bib <- bibentry(bibtype = "",
  #                 title = "",
  #                 author = person(""),
  #                 year = ,
  # )
  #
  # if(is.null(getOption("bibliography"))){
  #   options(bibliography = bib)
  # } else{
  #   currentBib <- getOption("bibliography")
  #   if(!bib%in%currentBib){
  #     options(bibliography = c(currentBib, bib))
  #   }
  # }

  return(out)

}

#' Generate a heightmap
#'
#' This is largely still work in progress.
#' 
#' A heightmap is the two dimensional representation of a three dimensional
#' surface, where the value of the cells represents the height in the three
#' dimensional surface (a simulated digitial elevation model).
#' @template mat
#' @param hurst [\code{numeric(1)}]\cr the Hurst exponent (fBm) or its
#'   equivalent, the roughness factor (DSa). Bounded between 0 and 1.
#' @param type [\code{character(1)}]\cr the model according to which the pattern
#'   is generated, either \code{"diamondSquare"} (default), \code{"brownian"} or
#'   \code{"gaussian"}.
#' @param startDev bla (for now)
#' @param seed bla
#' @details \describe{ \item{Diamond Square algorithm}{The algorithm was
#'   originally proposed by Fournier et al. (1982), but has been enhanced since
#'   then. It assigns values to an empty array (one or two dimensional).
#'   \emph{"The algorithm recursively subdivides the interval and generates a
#'   scalar value at the midpoint which is proportional to the current standard
#'   deviation times the scale or "roughness" factor (h). [...] h is equivalent
#'   to the Hurst exponent [see fractional brownian motion] and can take values
#'   between 0 and 1."} (Fournier et al, 1982). \cr\cr The implementation here
#'   computes values that are at the boundary of the two dimensional array as
#'   average from its three only neighbours and not from the one dimensional
#'   version of the algorithm (Fournier et al., 1982).}
#'   \item{Fractional brownian motion}{} \item{Gaussian random field}{} }
#' @references Fournier A, Fussell D, Carpenter L. Computer rendering of
#'   stochastic models. Communications of the ACM. 1982;25:371–384
#'
#'   Palmer MW. The coexistence of species in fractal landscapes. The American
#'   Naturalist. 1992;139:375–397
#'
#'   Travis JMJ, Dytham C. A method for simulating patterns of habitat
#'   availability at static and dynamic range margins. Oikos. 2004;104:410–416
#'
#' @examples
#' mat <- matrix(nrow = 100, ncol = 100, data = 0)
#' myHeightmap <- spmHeightmap(mat = mat, hurst = 0.4, seed = 13531)
#' visualise(myHeightmap)
#'
#' @importFrom checkmate assertMatrix assertCharacter assertNumeric assertIntegerish
#' @importFrom raster raster
#' @export

spmHeightmap <- function(mat, type = "diamondSquare", hurst = NULL, startDev = 1,
                         seed = NULL){

  # https://en.wikipedia.org/wiki/Brownian_surface
  # https://en.wikipedia.org/wiki/Random_walk
  
  assertMatrix(mat)
  assertCharacter(type, any.missing = FALSE, len = 1)
  assertNumeric(hurst, any.missing = FALSE, len = 1, lower = 0, upper = 1)
  assertIntegerish(seed, any.missing = FALSE, len = 1, null.ok = TRUE)

  if(type == "diamondSquare"){
    # manage the seed here.
    if(!is.null(seed)){
      if(length(seed) > 1){
        cornerSeed <- rep_len(seed, 4)
      } else{
        set.seed(seed)
        cornerSeed <- runif(4)
      }
    } else{
      cornerSeed <- runif(4)
    }
    
    # manage the initial array
    mat_dim <- max(dim(mat))
    level <- ceiling(log(mat_dim)/log(2)) 
    ext <- 2**level + 1
    targetMat <- matrix(data = 0, nrow = ext, ncol = ext)
    stepSize <- 2**(level:1)

    targetMat[1, 1] <- cornerSeed[1]
    targetMat[1, ext] <- cornerSeed[2]
    targetMat[ext, 1] <- cornerSeed[3]
    targetMat[ext, ext] <- cornerSeed[4]
    
    # this algorithm still needs a proper handling of the standard deviation.
    # The way it is now is not how it's supposed to be. (see link in cpp file)
    mat_out <- diamondSquareC(mat = targetMat, stepSize = stepSize, roughness = hurst, startDev = startDev)

    bib <- bibentry(bibtype = "Article",
                    author = c(person(given = "A", family = "Fournier"),
                               person(given = "D", family = "Fussell"),
                               person(given = "L", family = "Carpenter")),
                    title = "Computer rendering of stochastic models",
                    pages = "371-384",
                    year = 1982,
                    journal = "Communications of the ACM",
                    volume = 25,
                    issue = 6
    )
    
  } else if(type == "brownian"){
    stop("type = 'brownian' is not yet supported.")
    #   
  #   bib <- bibentry(bibtype = "Article",
  #                   author = c(person(given = "J M J", family = "Travis"),
  #                              person(given = "C", family = "Dytham")),
  #                   title = "A method for simulating patterns of habitat availability at static and dynamic range margins",
  #                   pages = "410-416",
  #                   year = 2004,
  #                   journal = "Oikos",
  #                   volume = 104,
  #                   issue = 2
  #                   )
  #   
  } else if(type == "gaussian"){
    stop("type = 'gaussian' is not yet supported.")
  }
  
  mat_out <- scaleMat(mat_out, c(0, 1))
  obj <- raster(mat_out, xmn = 0, xmx = ext, ymn = 0, ymx = ext)

  # # manage the bibliography entry (diamong-square algo or other)
  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  names(obj) <- paste0("heightmap_", type)
  return(obj)

}

# Keitt TH. 2000. Spectral representation of neutral landscapes. Landscape Ecology 15:479-493.
# Gardner RH, O'Neill R V, Turner MG, Dale VH. 1989. Quantifying scale-dependent effects of animal movement with simple percolation models. Landscape Ecology 3:217 - 227.
# Gustafson, E.J. & Parker, G.R. (1992) Relationships between landcover proportion and indices of landscape spatial pattern. Landscape Ecology , 7, 101 - 110.
# Schwab, Dimitri, Martin Schlather, and Jürgen Potthoff. "A general class of mosaic random fields." arXiv preprint arXiv:1709.01441 (2017). Baddeley, Adrian, Ege Rubak, and Rolf Turner. Spatial point patterns: methodology and applications with R. CRC Press, 2015.
# Gaucherel, C. (2008) Neutral models for polygonal landscapes with linear networks. Ecological Modelling, 219, 39 - 48.
#
