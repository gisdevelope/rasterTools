#' Generate (neutral) spatial patterns
#'
#' This function is still largely work in process and very experimental!
#' \code{generate} calls the operators of an algorithm to generate spatial
#' patterns.
#'
#' Spatial pattern models (SPMs), such as neutral landscape models (NLMs), are
#' useful to study an ecological response to a set of simulated spatial
#' patterns. Ideally, the resulting spatial patterns are strictly controlable
#' and in many situations they ought to be neutral (i.e. not influenced by any
#' process or bias). Here the term SPM is used because not all functions result
#' in neutral patterns.
#' @param model [\code{list(.)}]\cr algorithm in which the operators to generate
#'   spatial pattern models are specified. Each \code{operator} is a list
#'   iteself and includes the operator name and its arguments as sub-elements;
#'   see Examples.
#' @param dimensions [\code{integerish(2)}]\cr number of columns and rows the
#'   landscape model ought to have.
#' @details Many spatial pattern models have been suggested and NLMs probably
#'   constitute the most common use-case. However, often they are merely treated
#'   as "habitat models", where only the presence or abundance of habitat is
#'   simulated. Yet, "artificial patterns or processes" introduced by human
#'   disturbance and the influence of geological, atmospheric and ecological
#'   dynamics are also of interest. We try to cover these cases with ...
#'
#'   ... spatial pattern models: \itemize{ \item \code{\link{spmGradient}}:
#'   Generate a (neutral) gradient pattern. \item \code{\link{spmRandom}}:
#'   Generate a random pattern. \item \code{\link{spmHeightmap}}: Generate a
#'   heightmap. \item \code{spmNoise}: Generate patterns based on different
#'   kinds of noise.}
#'
#'   ... ecological processes models: \itemize{ \item \code{epmSucces}: Let
#'   objects in the spatial model success. \item \code{epmDiversify}: Let
#'   objects in the spatial model diversify. \item \code{epmPerforate}: Create
#'   gaps (e.g. clearcuts) into objects of the spatial model. \item
#'   \code{epmFragment}: Create fragmentation in the spatial model. \item
#'   \code{epmConnect}: Create connecting elements in the spatial model. }
#'
#'   Additionally you can use functions in other packages as operators, if they
#'   produce a 2D-lattice of spatial patterns; see for instance
#'   \href{https://github.com/ropensci/NLMR}{NLMR}.
#'
#' @examples
#' \dontrun{
#'
#' ## neutral landscape models
#' nlm <- list(list(operator = "spmRandom", seed = 12769),
#'             list(operator = "spmHeightmap", hurst = 0.4,
#'                  seed = 12769))
#'
#' myLandscape <- generate(model = nlm, dimensions = c(300, 300), to_env = TRUE)
#' }

generate <- function(model, dimensions){

  if(missing(dimensions)){
    stop("please specify the dimensions of the landscape model you would like to generate")
  }
  out <- model

  mat <- matrix(nrow = dimensions[1], ncol = dimensions[2], data = 0)



  return(out)
}
