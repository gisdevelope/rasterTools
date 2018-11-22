#' Manage the internal paths of rasterTools
#'
#' @param root [\code{character(1)}]\cr the root directory where all spatial
#'   files are already organised or should be saved to.
#' @param project [\code{character(1)}]\cr the project directory where the
#'   output of various functions will be stored to.
#' @param local [\code{list(.)}]\cr a named list pointing to the directory where
#'   the local resources are located.
#' @param remote [\code{list(.)}]\cr a named list pointing to the url where the
#'   remote resources are located.
#' @details When running the function \code{setPaths} the internal object
#'   \code{\link{rtPaths}} is update. In case \code{root} is given, this
#'   directory is searched for folders of all the specified datasets, and their
#'   path are assigned in \code{rtPaths}. With \code{local} or \code{remote},
#'   the local or online directory of a specific dataset can be updated.
#' @return Changes entries of the internal dataset \code{rtPaths}.
#' @examples
#' \dontrun{
#'
#' # run this when starting rasterTools for the first time
#' getOption("rtPaths")
#' setPaths(root = "/path/to/the/spatial/files")
#'
#' # in case a dataset is at an exotic locations
#' setPaths(local = list(sentinel = "/path/outside/of/root"))
#'
#' # change the (outdated) url of some datasets
#' setPaths(remote = list(sentinel = "https://new_url_to_sentinel/")
#'
#' getOption("rtPaths")
#' }
#' @export

setPaths <- function(root = NULL, project = NULL, local = NULL, remote = NULL){
  
  assertCharacter(root, null.ok = TRUE)
  assertCharacter(project, null.ok = TRUE)
  assertList(local, null.ok = TRUE)
  assertList(remote, null.ok = TRUE)
  
  rtPaths <- getOption("rtPaths")
  packageDataPath <- paste0(.libPaths()[1], "/rasterTools/data/")

  if(!is.null(root)){
    # remvove '/' at the end of path, in case it has been given
    if(substr(root, nchar(root), nchar(root))=="/"){
      root <- substr(root, 1, nchar(root)-1)
    }
    rtPaths$root <- root
  }
  if(!is.na(rtPaths$root)){
    dirs <- list.dirs(path = rtPaths$root, full.names = FALSE)
  } else{
    stop("please specify a root directory where the spatial files are located.")
  }
  if(is.null(project)){
    rtPaths$project <- paste0(rtPaths$root, "/rT_output")
  }
  projectDirExists <- testDirectoryExists(rtPaths$project)
  if(!projectDirExists){
    dir.create(rtPaths$project)
  }

  datasets <- names(rtPaths)[!names(rtPaths) %in% c("root", "project")]

  for(i in seq_along(datasets)){
    pos <- which(names(rtPaths) == datasets[i])

    if(!is.na(root)){
      thisDir <- dirs[grepl(pattern = paste0(datasets[i], "$"), x = dirs)]

      if(length(thisDir) == 0){
        thisDir <- datasets[i]
        message(paste0("I create the directory '", thisDir, "' in '", root, "'.\n"))
        dir.create(paste0(root, "/", thisDir))
      }
      rtPaths[[pos]]$local <- paste0(root, "/", thisDir)

    }
  }
  
  if(!is.null(local)){
    if(depthList(local) < 1 | is.null(names(local))){
      warning("I did not change anything, because 'local' is not properly specified.", call. = FALSE)
    } else{
      for(i in seq_along(names(local))){
        theDataset <- which(names(rtPaths) == names(local)[i])
        rtPaths[[theDataset]]$local <- local[[i]]
      }
    }
  }
  
  if(!is.null(remote)){
    if(depthList(remote) < 1 | is.null(names(remote))){
      warning("I did not change anything, because 'remote' is not properly specified.", call. = FALSE)
    } else{
      for(i in seq_along(names(remote))){
        theDataset <- which(names(rtPaths) == names(remote)[i])
        rtPaths[[theDataset]]$remote <- remote[[i]]
      }
    }
  }

  options(rtPaths = rtPaths)
  save(list = "rtPaths", file = paste0(packageDataPath, "rtPaths.rda"))
  unlockBinding(sym = "rtPaths", env = as.environment("package:rasterTools"))
  assign("rtPaths", rtPaths, envir = as.environment("package:rasterTools"))
}
