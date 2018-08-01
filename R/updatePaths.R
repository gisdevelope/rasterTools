#' Manage the internal paths of rasterTools
#'
#' @param root the root directory where all spatial files are already organised
#'   or should be saved to.
#' @param project the directory where your current project is stored. If left
#'   empty, it is assumed that it is the same directory as \code{root}.
#' @param set a named list pointing to the url where the online resources are
#'   located (see Details).
#' @details \code{rasterTools} includes an object \code{\link{rtPaths}}, which
#'   contains all paths that are currently relevant for \code{rasterTools} (i.e.
#'   where an 'obtain'-operator is defined). Each of the operators, but also
#'   other functions relying on paths take the paths from here.
#' @return Changes entries of the internal dataset \code{rtPaths}.
#' @examples
#' \dontrun{
#'
#' # run this when starting rasterTools for the first time
#' getOption("rtPaths")
#' updatePaths(root = "/path/to/the/spatial/files",
#'             project = "/path/to/your/project")
#'
#' # change the (outdated) url of some datasets
#' updatePaths(set = list(sentinel = "https://new_url_to_sentinel/",
#'                        gfc = "https://new_url_to_gfc/"))
#'
#' getOption("rtPaths")
#' }
#'
#' # updatePaths(root = "/media/steffen/36551F673A1E43DF/spatial/")
#' @export

updatePaths <- function(root, project, set){

  rtPaths <- getOption("rtPaths")

  packageDataPath <- paste0(.libPaths()[1], "/rasterTools/data/")
  if(missing(set)) set <- NULL

  if(!missing(root)){
    # remvove '/' at the end of path, in case it has been given
    if(substr(root, nchar(root), nchar(root))=="/"){
      root <- substr(root, 1, nchar(root)-1)
    }
    # # add "/" at the end of 'root' in case it's missing.
    # if(substr(root, nchar(root), nchar(root)) != "/"){
    #   root <- paste0(root, "/")
    # }
    rtPaths$root <- root
  } else if(is.na(rtPaths$root) & missing(root)){
    stop("Please define a 'root' directory.")
  }

  if(!missing(project)){
    rtPaths$project <- project
  } else if(is.na(rtPaths$project) & missing(project)){
    if(!missing(root)){
      rtPaths$project <- rtPaths$root
    } else{
      stop("Please define a 'project' or 'root' directory.")
    }
  }

  datasets <- names(rtPaths)[!names(rtPaths) %in% c("root", "project")]
  dirs <- list.dirs(path = rtPaths$root, full.names = FALSE)

  for(i in seq_along(datasets)){
    pos <- which(names(rtPaths) == datasets[i])

    if(is.na(rtPaths[[pos]]$local)){
      thisDir <- dirs[dirs %in% datasets[i]]

      if(length(thisDir) == 0){
        thisDir <- datasets[i]
        message(paste0("I create the directory '", thisDir, "' in '", root, "'.\n"))
        dir.create(paste0(root, "/", thisDir))
      }
      rtPaths[[pos]]$local <- paste0(root, "/", thisDir)

    }
  }

  if(!is.null(set)){

    if(depthList(set) < 1 | is.null(names(set))){
      warning("I did not change anything, because 'set' is not properly specified.", call. = FALSE)
    } else{
      for(i in seq_along(names(set))){
        theDataset <- which(names(rtPaths) == names(set)[i])
        rtPaths[[theDataset]]$online <- set[[i]]
      }
    }

  }

  options(rtPaths = rtPaths)
  save(list = "rtPaths", file = paste0(packageDataPath, "rtPaths.rda"))
  unlockBinding(sym = "rtPaths", env = as.environment("package:rasterTools"))
  assign("rtPaths", rtPaths, envir = as.environment("package:rasterTools"))
}
