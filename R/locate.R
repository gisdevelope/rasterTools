#' Locate (and identify) clicks
#'
#' Click into a plot to get the location or identify values
#' @param samples [\code{integerish(1)}]\cr the number of clicks.
#' @param gridded [\code{Raster*} | \code{matrix}]\cr the gridded object in
#'   which to locate.
#' @param panel [\code{character(1)}]\cr the panel in which to locate (i.e. the
#'   title shown over the plot). Does not have to be accurate, as it matches
#'   with \code{\link{grep}}.
#' @param identify [\code{logical(1)}]\cr get the value at the sampled location
#'   (\code{TRUE}) or merely the location (\code{FALSE}, default).
#' @param snap [\code{logical(1)}]\cr should the returned value(s) be set to the
#'   nearest grid cell's center (\code{TRUE}) or should they remain the
#'   selected, "real" value (\code{FALSE}, default)?
#' @param silent [\code{logical(1)}]\cr should all messages but errors be
#'   suppressed (\code{TRUE}), or should all messages be printed (\code{FALSE},
#'   default)?
#' @param show [\code{logical(1)}]\cr should information about the selected
#'   cells be included in the plot (\code{TRUE}), or should they merely be
#'   returned in the console (\code{FALSE, default})?
#' @param ... [\code{various}]\cr graphical parameters of the objects that are
#'   created when \code{show = TRUE}.
#' @return a \code{data.frame} of the selected locations and, if \code{identify
#'   = TRUE}, the respective values. If \code{show = TRUE} the values are also
#'   shown in the plot.
#' @examples
#' \dontrun{
#' input <- rtData$continuous
#' patches <- rPatches(rBinarise(input, thresh = 30))
#'
#' # locate from a not yet plotted object
#' locate(samples = 3, gridded = input, show = TRUE, col = "green")
#'
#' # or from one that is already plotted
#' visualise(gridded = patches)
#' locate(identify = TRUE, snap = TRUE)
#'
#' # with several panels, specify a target
#' visualise(gridded = raster::brick(input, patches))
#' locate(samples = 4, panel = "patches", identify = TRUE)
#' }
#' @importFrom grDevices dev.list
#' @importFrom grid grid.ls grid.grep grid.force seekViewport grid.locator gList
#'   pointsGrob textGrob grid.draw upViewport unit grid.get gPath
#' @importFrom raster as.matrix
#' @export

locate <- function(samples = 1, gridded = NULL, panel = NULL, identify = FALSE,
                   snap = FALSE, silent = FALSE, show = FALSE, ...){

  # check arguments
  assertIntegerish(samples, lower = 1, max.len = 1)
  isRaster <- testClass(gridded, "Raster")
  isMatrix <- testClass(gridded, "matrix")
  existsGridded <- ifelse(c(isRaster | isMatrix), TRUE, FALSE)
  assertCharacter(panel, ignore.case = TRUE, len = 1, null.ok = TRUE)
  assertLogical(identify, len = 1)
  assertLogical(snap, len = 1)
  assertLogical(silent, len = 1)
  assertLogical(show, len = 1)

  # test whether a rasterTools plot is already open
  if(!is.null(dev.list())){
    objViewports <- grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
    mainVP <- grid.grep("vpLomm", grobs = FALSE, viewports = TRUE, grep = TRUE)
    isOpenPlot <- ifelse(any(mainVP == "vpLomm"), TRUE, FALSE)

    panelNames <- objViewports$name[objViewports$vpDepth == 2 & objViewports$name != "1"]
    panelNames <- panelNames[!duplicated(panelNames)]
  } else{
    isOpenPlot <- FALSE
  }

  if(isOpenPlot){
    if(existsGridded){
      # if both are given, check whether their names are the same. If not, prepare
      # to plot obj
      if(!all(names(gridded) == panelNames)){
        panelNames <- names(gridded)
        visualise(gridded = gridded)
      }
    } else{
      isLegendInPlot <- !identical(grid.grep("legend", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
      isRasterInPlot <- !identical(grid.grep("raster", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
      if(identify & !(isLegendInPlot & isRasterInPlot)){
        stop("to identify values, please provide either a legend or 'gridded'.")
      }
    }
  } else{
    if(!existsGridded){
      stop("please either provide 'gridded' or create a plot with rasterTools::visualise().")
    } else{
      panelNames <- names(gridded)
      visualise(gridded = gridded)
    }
  }


  # get the panel in which locations should be determined
  if(is.null(panel)){
    if(length(panelNames) > 1){
      warning("no panel has been specified, so I chose the first panel. Please select locations there.", immediate. = TRUE, call. = FALSE)
    }
    panel <- panelNames[1]
  } else{
    panel <- panelNames[grepl(panel, panelNames)]
    if(length(panel) == 0){
      panel <- panelNames[1]
      warning("the specified panel did not match any of the existing panels, so I chose the first panel. Please select locations there.", immediate. = TRUE, call. = FALSE)
    }
  }

  # find the correct viewport to limit actions to this area of the plot
  panelvpPath <- grid.grep(paste0(panel, "::plot::grid::raster"), grobs = FALSE, viewports = TRUE, grep = TRUE)
  seekViewport(panelvpPath)

  # get ranges of the x and y-axis (that is the range of the background grid,
  # exceeding the actual range of the raster)
  extentGrobMeta <- grid.get(gPath("extentGrob"))
  panelExt <- c(xMin = 0, xMax = as.numeric(extentGrobMeta$width),
                yMin = 0, yMax = as.numeric(extentGrobMeta$height))

  if(identify){
    if(!existsGridded){
      metaRaster <- grid.get(gPath("theRaster"), global = TRUE)
      metaLegend <- grid.get(gPath("theLegend"), global = TRUE)
      metaTicks <- grid.get(gPath("legendTicks"), global = TRUE)
      if(length(panelNames) > 1){
        mat <- as.matrix(metaRaster[which(panel == panelNames)][[1]]$raster)
        legend <- metaLegend[which(panel == panelNames)][[1]]$raster
        ticks <- as.numeric(rev(metaTicks[which(panel == panelNames)][[1]]$label))
      } else{
        mat <- as.matrix(metaRaster$raster)
        legend <- metaLegend$raster
        ticks <- as.numeric(rev(metaTicks$label))
      }

      mat <- subChrIntC(mat,
                        replace = legend,
                        with = ticks)
    } else{
      if(isRaster){
        mat <- as.matrix(eval(parse(text = paste0("gridded$", panel))))
      } else{
        mat <- gridded
      }
    }
  }

  if(snap){
    theGrid <- data.frame(x = rep(seq(0.5, panelExt[[2]], 1), times = panelExt[[4]]),
                          xmin = rep(seq(0, panelExt[[2]]-1), times = panelExt[[4]]),
                          xmax = rep(seq(1, panelExt[[2]]), times = panelExt[[4]]),
                          y = rep(seq(panelExt[[4]]-0.5, 0), each = panelExt[[2]]),
                          ymin = rep(seq(panelExt[[4]]-1, 0), each = panelExt[[2]]),
                          ymax = rep(seq(panelExt[[4]], 1), each = panelExt[[2]]))
  }

  if(!silent){
    message(paste0("please click ", samples, " location(s) in the panel '", panel, "'.\n\n"))
  }

  out <- NULL
  for(i in 1:samples){
    click <- grid.locator()

    values <- round(as.numeric(click), 1)
    if(any(values < 0)) values <- c(NA, NA)

    # values need to be rescaled to the dimension of the marked window.
    # values[1] <- round(((max(xRange) - min(xRange))*(values[1] - 0) / (1 - 0)) + min(xRange), 1)
    # values[2] <- round(((max(yRange) - min(yRange))*(values[2] - 0) / (1 - 0)) + min(yRange), 1)

    # snap to the middle of the selected raster cells
    if(snap){
      theLocations <- theGrid[which(values[1] > theGrid$xmin & values[1] <= theGrid$xmax &
                                         values[2] > theGrid$ymin & values[2] <= theGrid$ymax),c(1, 4)]
    } else{
      theLocations <- data.frame(cbind(x = values[1], y = values[2]))
    }

    if(identify){
      theValue <- mat[round(panelExt[[4]]+1-values[2]), round(values[1])]
      out <- data.frame(rbind(out, c(x = theLocations$x, y = theLocations$y, id = i, value = theValue)))
    } else{
      out <- data.frame(rbind(out, c(x = theLocations$x, y = theLocations$y, id = i)))
    }

    if(show){
      if(identify){
        toDraw <- gList(pointsGrob(x = click$x,
                                   y = click$y,
                                   pch = 16,
                                   size = unit(1, "mm"),
                                   gp = gpar(...)),
                        textGrob(paste0("[", theValue, "]"),
                                 click$x + unit(2, "mm"),
                                 click$y,
                                 just = "left",
                                 check.overlap = TRUE,
                                 gp = gpar(...))
        )
      } else{
        toDraw <- gList(pointsGrob(x = click$x,
                                   y = click$y,
                                   pch = 16,
                                   size = unit(1, "mm"),
                                   gp = gpar(...)),
                        textGrob(paste0("(", values[1], ", ", values[2], ")"),
                                 click$x + unit(2, "mm"),
                                 click$y,
                                 just = "left",
                                 check.overlap = TRUE,
                                 gp = gpar(...)))
      }
      grid.draw(toDraw)
    }

  }
  upViewport(4)

  return(out)
}
