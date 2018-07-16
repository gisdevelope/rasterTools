#' Build a gradient landscape model
#'
#' A gradient can be understood in terms of the distance to an origin. With a
#' straight line as origin, one might end up with a edge gradient, or, in case
#' the line does not cross the the landscape, a planar gradient.
#' @template mat
#' @param origin [\code{RasterLayer} | \code{matrix}]\cr a binary object that
#'   serves as origin of the gradient; overrides the other arguments, if given.
#' @param type [\code{¢haracter(1)}]\cr the type of the gradient model. Either
#'   \code{"planar"} (default), \code{"point"}, \code{"line"}, \code{"polygon"},
#'   special cases thereof or \code{"random"}, to select one by chance.
#' @template params
#' @details In case \code{origin} is not given, \code{nlmGradient} constructs
#'   internally a binary origin from what is specified in \code{type} and
#'   \code{params} and then provides a distance matrix as gradient.
#' @examples
#' # create a point gradient based on an origin
#' mat <- matrix(nrow = 100, ncol = 100, data = 0)
#' origin <- mat; origin[5000] <- 1
#' myPointGradient <- nlmGradient(mat = mat, origin = origin)
#' visualise(gridded = myPointGradient)
#'
#' # create a geometry object
#' coords <- list(coords = data.frame(x = c(5, 30, 30, 5),
#'                                    y = c(5, 5, 20, 20)),
#'                extent = data.frame(x = c(0, 100),
#'                                    y = c(0, 50)))
#'
#' # create gradient from a polygon mask with the respective params
#' #myNLM <- nlmGradient(mat = mat, type = "polygon",
#'  #                    params = list(anchor = coords, show = FALSE))
#' #visualise(myNLM)
#' @importFrom checkmate assertMatrix testClass assertCharacter assertSubset
#'   assertList
#' @importFrom raster raster as.matrix
#' @export

nlmGradient <- function(mat, origin = NULL, type = "planar", params = NULL){

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
  assertList(params, names = "named", null.ok = TRUE)

  if(!existsOrigin){
    if(type == "planar"){
      # create a linear line geom and make a gradient orthogonal to that and starting somewhere outside the window
      
    } else{
      if(type == "polygon"){
        if(is.null(params)){
          verts <- readline("please specify how many vertices the polygon shall have: ")
          verts <- assertIntegerish(as.integer(verts))
        }
      }
      if(is.null(params)){
        # if()
        # theGeom <- geomRand(type = type, template = mat, vertices = verts)
        # theMask <- gToRaster(theGeom)
      } else{
        
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
  temp <- scaleVals(temp, c(0, 1))

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

#' Build a random landscape model
#'
#' Random values can be applied to different patterns within the landscape
#' model.
#' @param mat a matrix in which the landscape model is built. If
#'   \code{nlmRandom} is used in conjunction with \code{\link{generate}},
#'   \code{mat} cannot be specified, because \code{generate} assigns it.
#' @param pattern the pattern to which random values are assigned, either
#'   \code{"cell"}, \code{"rectangle"}, \code{"circle"}, \code{"voronoi"} or
#'   \code{"cluster"}; see Details.
#' @param seed specify a seed for the random creation of numbers.
#' @details \itemize{ \item For \code{base = "cell"}, each cell of the resulting
#'   LM is assigned a random value. \item For \code{base = "rectangle"}, \item
#'   For \code{base = "circle"}, \item For \code{base = "voronoi"}, Gaucherel
#'   (2008) \item For \code{base = "cluster"}, Saura & Martínez-Millán (2000) }
#' @references Gaucherel, C. (2008) Neutral models for polygonal landscapes with
#'   linear networks. Ecological Modelling, 219, 39 - 48. Saura, S.,
#'   Martínez-Millán, J. (2000) Landscape patterns simulation with a modified
#'   random clusters method. Landscape Ecology 15, 661–678.
#' @importFrom raster raster

nlmRandom <- function(mat, pattern, seed){

  # https://en.wikipedia.org/wiki/Centroidal_Voronoi_tessellation


  pattern <- match.arg(arg = pattern, choices = c("cell", "rectangle", "circle", "voronoi", "cluster"))

  if(!missing(seed)){
    set.seed(seed)
  }

  if(pattern == "cell"){
    values <- runif(length(mat), min = 0, max = 1)
    mat[] <- values
  } else if(pattern == "rectangle"){

  } else if(pattern =="circle"){

  } else{

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

#' Build a landscape model based on a heightmap
#'
#' A heightmap is the two-dimensional representation of a three-dimensional
#' surface, where the value of the heightmap represents the height in the
#' three-dimensional surface (like a digitial elevation model).
#' @param mat bla
#' @param type the model used to create the heightmap. Possible options are
#'   \code{diamondSquare} (default), ...
#' @param fracdim bla
#' @param seed bla
#' @importFrom raster raster

nlmHeightmap <- function(mat, type, fracdim, seed){

  # https://en.wikipedia.org/wiki/Brownian_surface
  # https://en.wikipedia.org/wiki/Random_walk

  if(missing(fracdim)) stop("please specify the fractal dimension for which you would like to create a heightmap.")

  # manage the seed here.
  if(!missing(seed)){
    if(length(seed)>1){
      cornerSeed <- rep_len(seed, 4)
    } else{
      set.seed(seed)
      cornerSeed <- runif(4)
    }
  } else{
    cornerSeed <- runif(4)
  }

  # manage the initial array
  mat_dim <- max(dim(mat)) # the algorithm works only for squares, hence we need the larges dimension.
  level <- ceiling(log(mat_dim)/log(2)) # ceiling to find the power which leads to a raster larger than what has been defined in 'dimensions'
  ext <- (2**level)+1
  mat_out <- matrix(data = 0, nrow = ext, ncol = ext)

  mat_out[1, 1] <- cornerSeed[1]
  mat_out[1, ext] <- cornerSeed[2]
  mat_out[ext, 1] <- cornerSeed[3]
  mat_out[ext, ext] <- cornerSeed[4]

  for(i in seq_len(level)){

    distance <- 1 # this probably doesn't make sense, but atm in here to silence the NOTES of R CMD CHECK
    cells <- (ext-1)/2**(i-1)
    steps <- floor(ext/distance)
    points <- seq.int(from = 1, by = cells, length.out = steps)

    magnitude <- 32/2**(i-1) # why 32 here? where does the fractal dimension come in?

    mat_out <- diamondC(mat = mat_out, where = points, distance = cells, noise = magnitude)
    mat_out <- squareC(mat = mat_out, where = points, distance = cells, noise = magnitude)

  }
  # raster::plot(:raster(mat_out, xmn = 0, xmx = ext, ymn = 0, ymx = ext))

  # cut 'out' to 'mat_dim' in a random spot, this could be goverened by an argument such as subset = c("random", c(x-center, y-center))

  obj <- raster(mat_out, xmn = 0, xmx = ext, ymn = 0, ymx = ext)

  # # manage the bibliography entry (diamong-square algo or other)
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

  return(obj)

}

# Keitt TH. 2000. Spectral representation of neutral landscapes. Landscape Ecology 15:479-493.
# Gardner RH, O'Neill R V, Turner MG, Dale VH. 1989. Quantifying scale-dependent effects of animal movement with simple percolation models. Landscape Ecology 3:217 - 227.
# Gustafson, E.J. & Parker, G.R. (1992) Relationships between landcover proportion and indices of landscape spatial pattern. Landscape Ecology , 7, 101 - 110.
# Schwab, Dimitri, Martin Schlather, and Jürgen Potthoff. "A general class of mosaic random fields." arXiv preprint arXiv:1709.01441 (2017). Baddeley, Adrian, Ege Rubak, and Rolf Turner. Spatial point patterns: methodology and applications with R. CRC Press, 2015.
# Gaucherel, C. (2008) Neutral models for polygonal landscapes with linear networks. Ecological Modelling, 219, 39 - 48.
#

