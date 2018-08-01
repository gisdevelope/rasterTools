#' List of references
#'
#' Put together a list of references (a bibliography) of all the used datasets
#' and R-packages
#' @param outFile [\code{character(1)}]\cr an optional file to which the
#'   bibliography should be saved.
#' @param print [\code{logical(1)}]\cr should the output be printed in the
#'   console (\code{TRUE}, defailt) or should it merely be returned
#'   (\code{FALSE})?
#' @param style [\code{character(1)}]\cr the print style (by default
#'   \code{"bibtex"}). If present, must be a unique abbreviation (with case
#'   ignored) of the available styles, see \code{\link[utils]{bibentry}}.
#' @param ... [various]\cr additional arguments for the \code{format()} method
#'   of \code{bibentry}.
#' @return A list of all the references that result from the operations that
#'   were used in the recent session.
#' @examples
#' # just return the list to the console ...
#' reference(style = "latex")
#'
#'\dontrun{
#'
#' # ... or store it in your project path
#' reference(outFile = "myBib.bib")
#'}
#' @importFrom utils bibentry person citation
#' @export

reference <- function(outFile = NULL, print = TRUE, style = "bibtex", ...){

  outFileExists <- testCharacter(outFile, len = 1, any.missing = FALSE)
  if(outFileExists){
    if(length(strsplit(outFile, "/")[[1]]) == 1){
      outFile <- paste0(getwd(), outFile)
    }
  }

  bib <- getOption("bibliography")

  rtbib <- c(citation(),
             bibentry(bibtype = "Manual",
                      title = "rasterTools: obtain and process earth observation data",
                      author = person(given = "Steffen", family = "Ehrmann",
                                      email = "steffen.science@funroll-loops.de",
                                      role = c("aut", "cre")),
                      url = "https://ehrmanns.github.io/rasterTools",
                      note = paste0("version ", packageVersion("rasterTools")),
                      year = 2018)
  )

  if(is.null(bib)){
    bib <- rtbib
  } else {
    if(!any(bib%in%rtbib)){
      bib <- c(bib, rtbib)
    }
  }

  if(!is.null(outFile)){
    writeLines(text = format(bib, style, ...), con = outFile)
  } else{
    if(print){
      print(bib, style)
    } else{
      return(bib)
    }
  }

}
