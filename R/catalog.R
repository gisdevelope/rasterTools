#' Build a look-up-table (catalog)
#'
#' \code{catalog} aims to make working with a wide variety of complex names
#' simpler. Complex terms such as species, territorial units, expriment names,
#' ... are related to systematic abbreviations thereof.
#' @param path [\code{character(1)}]\cr the path to a folder that contains files
#'   for which you want to build a catalog.
#' @param abbreviateBy [\code{function}]\cr to translate the original names to
#'   abbreviations thereof; see Examples.
#' @param type [\code{character(1)}]\cr the file type for which should be
#'   filtered.
#' @param recursive [\code{logical(1)}]\cr should the listing recurse into sub
#'   directories (\code{TRUE}) or should only the specified directory be
#'   searched (\code{FALSE}, default)?
#' @param inclType [\code{logical(1)}]\cr should the file-extension be included
#'   in the recorded original names (\code{TRUE}), or should the file name be
#'   included without extension (\code{FALSE}, default)?
#' @param silent [\code{logical(1)}]\cr should all messages but errors be
#'   suppressed (\code{TRUE}), or should all messages be printed (\code{FALSE},
#'   default)?
#' @details Often data-files come with complicated names, characterising in
#'   large detail the content. For a quick workflow, this is often a hindrance
#'   and not required, because a shorter yet systematic name can also
#'   sufficiently reference files. Typical examples would be: \itemize{ \item
#'   three or four-letter code for species (apo_fla, myo_gla, ...). \item
#'   two-letter NUTS code of territorial units (EE, DE, ...). \item a short name
#'   of some repeated experiment, originally labelled by
#'   date_region_researcher_treatment_etc (S11, S12, ...). \item ... } If you
#'   want to work with a relatively large amount of such files on our hard
#'   drive, it would be desirable to have some sort of index available, which
#'   makes it possible to readily access everything without having to load it to
#'   the global environment beforehand. This function tries to help here, by
#'   iteratively going through all directories which are specified in
#'   \code{path}, registering the files (incl. \code{path}) of the specified
#'   \code{type} and deriving a name based on the function defined in
#'   \code{abbreviateBy}. These information are stored in the \code{data.frame}
#'   \code{myCatalog} in the gloabl environment so that the objects in
#'   \code{path} can be quickly loaded by referring to their abbreviations.
#'
#'   The recorded original names are internally saved as character vector with
#'   the name \code{files} and \code{i} is used as iterator through its
#'   elements. The only limitation for this function hence is that the
#'   abbreviation must be some derivative of the original filenames, employing
#'   character manipulation functions such as \code{\link{substr}},
#'   \code{\link{strsplit}}, \code{\link{toupper}}, \code{\link{paste0}} or any
#'   other.
#' @return A \code{data.frame} with a column of original names and of the
#'   associated abreviations.
#' @examples
#' \dontrun{
#'
#' # build a catalog for all species for which there are maps in the 'Atlas of
#' # European mammals'
#'
#' # 1. create character variable which corresponds to your file names
#' x <- "Vulpes vulpes.svg"
#'
#' # 2. combine with character manipulation functions until the pattern matches
#' strsplit(x, ' ')
#' unlist(strsplit(x, ' '))
#' substr(unlist(strsplit(x, ' ')), 1, 3)
#' paste(substr(unlist(strsplit(x, ' ')), 1, 3), collapse = '_')
#' tolower(paste(substr(unlist(strsplit(x, ' ')), 1, 3), collapse = '_'))
#'
#' # 3. turn this into a function and use as value in abbreviateBy.
#' abbr_species <- function(x){
#'   tolower(paste(substr(unlist(strsplit(x, ' ')), 1, 3), collapse = '_'))
#' }
#'
#' require(magrittr)
#' catalog(path = rtPaths$emma$local, type = 'svg', inclType = TRUE,
#'     abbreviateBy = abbr_species) %>%
#'   subset(abbr %in% c("apo_agr", "apo_fla", "vul_vul"))
#' }
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

catalog <- function(path = ".", abbreviateBy = NULL, type = NULL, recursive = FALSE,
                    inclType = FALSE, silent = FALSE){
  
  assertDirectory(path, access = "r")
  assertFunction(abbreviateBy, args = c("x"), nargs = 1)
  assertCharacter(type, min.chars = 3, ignore.case = TRUE, any.missing = FALSE, max.len = 1, null.ok = TRUE)
  assertLogical(recursive)
  assertLogical(inclType)
  if(!is.null(type)){
    files <- list.files(path, pattern = paste0("[.]", type, "$"), recursive = recursive)
    if(!inclType){
      files <- substr(files, 1, nchar(files)-(nchar(type)+1))
    }
  } else{
    files <- list.files(path, recursive = recursive)
  }

  # in case recursive is activated, strip of the path and work only with the
  # actual file names for abbreviations.
  if(length(files) == 0){
    stop(paste0("I did not find any suitable files in '", path, "'."), call. = FALSE)
  } else{
    
    if(recursive){
      split_files <- strsplit(files, split = "/")
      file_names <- unlist(lapply(
        seq_along(files), function(i){
          theFile <- strsplit(files[i], split = "/")[[1]]
          theFile <- theFile[length(theFile)]
        }
      ))
    } else{
      file_names <- files
    }

    if(!silent){
      pb <- txtProgressBar(min = 0, max = length(files), style = 3, char=">", width=getOption("width")-14)
    }

    for(i in seq_along(files)){
      x <- c(files[i], sapply(file_names[i], abbreviateBy, USE.NAMES = FALSE))

      if(i == 1){
        df <- tibble("original" = x[1], "abbr" = x[2])
      } else{
        temp <- tibble("original" = x[1], "abbr" = x[2])
        df <- bind_rows(df, temp)
      }

      if(!silent){
        setTxtProgressBar(pb, i)
      }
    }
    if(!silent){
      close(pb)
    }
  }

  return(df)
}
