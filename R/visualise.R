#' Visualise raster and geom objects
#'
#' @param raster [\code{Raster*} | \code{matrix}]\cr raster object to plot.
#' @param geom [\code{geom}]\cr geom to plot.
#' @param theme [\code{list(7)}]\cr Visualising options; see
#'   \code{\link{setTheme}} for details.
#' @param trace [\code{logical(1)}]\cr Print the raster object's history (i.e.
#'   the process according to which it has been created) (\code{TRUE}), or
#'   simply plot the object (\code{FALSE}, default).
#' @param image [\code{logical(1)}]\cr Does \code{raster} have the channels
#'   \code{red}, \code{green} and \code{blue}, i.e. is it an "image"
#'   (\code{TRUE}) or is this not the case (\code{FALSE}, default)?
#' @param new [\code{logical(1)}]\cr force a new plot (\code{TRUE}, default).
#' @param ... [various]\cr Graphical parameters to \code{geom}.
#' @details To create a plot with your own style, design it with
#'   \code{\link{setTheme}} and use it in \code{theme}.
#'
#'   In case you want to plot an image (simiar to
#'   \code{\link[raster]{plotRGB}}), you have to provide a \code{RasterStack} or
#'   \code{RasterBrick} with the three layers \code{red}, \code{green} and
#'   \code{blue} and set \code{image = TRUE}.
#' @return Returns invisibly an object of class \code{recordedplot}, see
#'   \code{\link{recordPlot}} for details (and warnings).
#' @examples
#' input <- rtData$continuous
#' binarised <- rBinarise(input, thresh = 40)
#' visualise(raster = rDistance(binarised), trace = TRUE)
#'
#' # visualise also RasterBrick/-Stack objects
#' getDistances <- list(disEuc = list(operator = "rDistance"),
#'                      disMht = list(operator = "rDistance",
#'                                    method = "manhattan"),
#'                      disChb = list(operator = "rDistance",
#'                                    method = "chessboard"))
#' distances <- modify(input = binarised, by = getDistances, merge = TRUE)
#' distances <- raster::brick(binarised, distances)
#' visualise(distances)
#'
#' # define a geometry
#' coords <- data.frame(x = c(30, 60, 60, 40),
#'                      y = c(40, 40, 60, 70),
#'                      id = 1)
#' window <- data.frame(x = c(0, 80),
#'                      y = c(0, 80))
#' (aGeom <- geomPolygon(anchor = coords, window = window, col = "blue"))
#'
#' # if plotted on top of an existing plot, the relative coordinate values
#' # will be used to construct the grob.
#' visualise(raster = input, geom = aGeom)
#' visualise(geom = aGeom, new = TRUE)
#'
#' @importFrom checkmate testClass testList assertNames assertList assertLogical
#'   testCharacter testIntegerish
#' @importFrom grid grid.newpage pushViewport viewport grid.rect grid.raster
#'   grid.clip unit grid.draw grid.grill upViewport grid.text gpar convertX
#'   downViewport
#' @importFrom grDevices colorRampPalette as.raster recordPlot rgb
#' @importFrom raster nlayers values as.matrix ncol nrow
#' @export

visualise <- function(raster = NULL, geom = NULL, theme = NULL, trace = FALSE,
                      image = FALSE, new = TRUE, ...){

  # raster = raster::stack(input, substituted); geom = NULL; theme = NULL; trace = FALSE; image = FALSE; new = TRUE
  
  # new ideas:
  # 1. automatically detect which is raster and which is geom
  # 2. Rcpp for the gScale and gToGrob functions
  # 3. enable colouring of geom based on its values
    
  # check arguments
  isRaster <- testClass(raster, "Raster")
  isRasterStackBrick <- testClass(raster, "RasterStackBrick")
  isMatrix <- testClass(raster, "matrix")
  existsGridded <- ifelse(c(isRaster | isMatrix), TRUE, FALSE)
  existsGeom <- testClass(geom, classes = "geom")
  if(!existsGeom & !is.null(geom)){
    stop("please provide a valid 'geom' object to plot.")
  }
  if(!existsGridded & !existsGeom){
    stop("please provide either a raster object or a geometry to plot.")
  }
  assertList(theme, len = 7, null.ok = TRUE)
  if(is.null(theme)){
    theme <- theme_rt
  } else{
    assertNames(names(theme), permutation.of = c("plot", "labels", "bins", "margin", "scale", "legend", "par"))
  }
  assertLogical(trace)
  assertLogical(image)
  assertLogical(new)

  # check whether a plot is already open and whether it is valid
  if(!is.null(dev.list()) & !new){
    objViewports <- grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
    isOpenPlot <- ifelse(any(objViewports$name == "vpLomm"), TRUE, FALSE)

    panelNames <- objViewports$name[objViewports$vpDepth == 2 & objViewports$name != "1"]
    panelNames <- panelNames[!duplicated(panelNames)]
  } else{
    isOpenPlot <- FALSE
  }

  # turn raster into an array and extract meta-data
  if(existsGridded){
    if(isRaster){

      plotLayers <- nlayers(raster)
      griddedNames <- names(raster)
      vals <- lapply(1:plotLayers, function(x){
        as.vector(raster[[x]])
      })
      uniqueVals <- lapply(1:plotLayers, function(x){
        sort(unique(vals[[x]], na.rm = TRUE))
      })
      dims <- dim(raster[[1]])
      ext <- extent(raster[[1]])
      panelExt <- c(xMin = ext@xmin, xMax = ext@xmax, yMin = ext@ymin, yMax = ext@ymax)
      hasColourTable <- lapply(1:plotLayers, function(x){
        ifelse(length(raster[[x]]@legend@colortable) > 0, TRUE, FALSE)
      })
      isFactor <- lapply(1:plotLayers, function(x){
        ifelse(raster[[x]]@data@isfactor, TRUE, FALSE)
      })

    } else if(isMatrix){

      plotLayers <- 1
      griddedNames <- "layer"
      vals <- list(getValuesMatC(raster))
      uniqueVals <- list(sort(unique(vals[[1]], na.rm = TRUE)))
      dims <- dim(raster)
      panelExt <- c(xMin = 0, xMax = ncol(raster), yMin = 0, yMax = nrow(raster))
      hasColourTable <- FALSE
      isFactor <- FALSE
      
    }
    
    # checks in case raster is supposed to be an "image"
    if(image){
      assertNames(griddedNames, permutation.of = c("red", "green", "blue"))
      assertIntegerish(plotLayers, lower = 3, upper = 3)
      plotLayers <- 1
    }
    
  }

  if(isOpenPlot){
    if(existsGridded){
      # if both are given, check whether their names are the same. If not, prepare
      # to plot raster
      if(!all(panelNames == griddedNames)){
        isOpenPlot <- FALSE
        panelNames <- griddedNames
      }
    } else if(!existsGridded){
      plotLayers <- length(panelNames)
    }
  } else{
    if(existsGridded){
      panelNames <- griddedNames
    }
  }

  # if plot still valid, determine which elements are available
  if(isOpenPlot){
    isTitleInPlot <- !identical(grid.grep("title", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
    isLegendInPlot <- !identical(grid.grep("legend", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
    isYAxisInPlot <- !identical(grid.grep("yAxisTitle", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
    isXAxisInPlot <- !identical(grid.grep("xAxisTitle", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
    isRasterInPlot <- !identical(grid.grep("raster", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
    isGeomInPlot <-!identical(grid.grep("geom", grobs = FALSE, viewports = TRUE, grep = TRUE), character(0))
  } else{
    isTitleInPlot <- FALSE
    isLegendInPlot <- FALSE
    isYAxisInPlot <- FALSE
    isXAxisInPlot <- FALSE
    isRasterInPlot <- FALSE
    isGeomInPlot <-FALSE
  }

  # override legend in some cases
  if(isGeomInPlot & !isRasterInPlot | image){
    theme$plot$legend <- FALSE
  }

  # turn 'geom' into a grob that can be plotted
  if(existsGeom){
    
    if(isOpenPlot){
      # if a plot is already open, we want to get it's extent so that the
      # relative coordinates of geom can be calculated correctly.
        extentGrobMeta <- grid.get(gPath("extentGrob"))
        panelExt <- c(xMin = 0, xMax = as.numeric(extentGrobMeta$width),
                      yMin = 0, yMax = as.numeric(extentGrobMeta$height))

        d <- getOption("digits")
        options(digits = 15)

        xAxisMeta <- grid.get(gPath("xAxisTicks"))
        yAxisMeta <- grid.get(gPath("yAxisTicks"))
        xTickLabels <- as.numeric(xAxisMeta$label)
        yTickLabels <- as.numeric(yAxisMeta$label)
        geom@window <- data.frame(x = rep(c(min(xTickLabels), max(xTickLabels)), each = 2),
                                  y = c(min(yTickLabels), max(yTickLabels), max(yTickLabels), min(yTickLabels)))

        options(digits = d)
    } else{
      if(!existsGridded){
        panelExt <- c(xMin = min(geom@window$x), xMax = max(geom@window$x),
                      yMin = min(geom@window$y), yMax = max(geom@window$y))
        plotLayers <- 1
        panelNames <- geom@type
      }
    }

    geomGrob <- gToGrob(geom = geom, theme = theme, ...)
  }

  # checkup concerning plot size
  if(plotLayers > 30){
    question <- readline(paste0("  -> this will produce ", plotLayers, " plots, do you wish to continue? [yes/no]: "))
    question <- match.arg(question, c("yes", "no"))
    if(question == "no"){
      return(invisible(0))
    }
  }

  # manage plot properties
  ratio <- list(x = (panelExt[[2]] - panelExt[[1]])/(panelExt[[4]] - panelExt[[3]]),
                y = (panelExt[[4]] - panelExt[[3]])/(panelExt[[2]] - panelExt[[1]]))
  xBins <- theme$bins$xAxis
  yBins <- theme$bins$yAxis
  xBinSize <- (panelExt[[2]] - panelExt[[1]])/xBins
  yBinSize <- (panelExt[[4]] - panelExt[[3]])/yBins
  axisSteps <- list(x1 = seq(from = panelExt[[1]],
                             to = panelExt[[2]],
                             by = (panelExt[[2]] - panelExt[[1]])/xBins),
                    x2 = seq(from = panelExt[[1]] + (xBinSize/2),
                             to = panelExt[[2]],
                             by = (panelExt[[2]] - panelExt[[1]])/xBins),
                    y1 = seq(from = panelExt[[3]],
                             to = panelExt[[4]],
                             by = (panelExt[[4]] - panelExt[[3]])/yBins),
                    y2 = seq(from = panelExt[[3]] + (yBinSize/2),
                             to = panelExt[[4]],
                             by = (panelExt[[4]] - panelExt[[3]])/yBins))
  margin <- list(x = (panelExt[[2]]-panelExt[[1]])*theme$margin$xAxis,
                 y = (panelExt[[4]]-panelExt[[3]])*theme$margin$yAxis)

  # manage the colours
  if(existsGridded){
    
    if(image){
      red <- as.integer(vals[[which(panelNames == "red")]])
      red[is.na(red)] <- 255L
      green <- as.integer(vals[[which(panelNames == "green")]])
      green[is.na(green)] <- 255L
      blue <- as.integer(vals[[which(panelNames == "blue")]])
      blue[is.na(blue)] <- 255L
      
      theColours <- list(rgb(red = red, green = green, blue = blue, maxColorValue = 255))
      panelNames <- "image"
    } else{
      uniqueColours <- lapply(seq_along(uniqueVals), function(x){
        tempVals <- uniqueVals[[x]]
        nrVals <- length(tempVals)
        if(nrVals < 256){
          nrColours <- nrVals
        } else{
          nrColours <- 256
        }
        if(hasColourTable[[x]]){
          raster[[x]]@legend@colortable[tempVals]
        } else{
          colorRampPalette(colors = theme$scale$raster$colours)(nrColours)
        }
      })
      
      if(theme$plot$commonScale){
        uniqueVals <- lapply(seq_along(uniqueVals), function(x){
          sort(unique(unlist(uniqueVals), na.rm = TRUE))
          
        })
      }

      theColours <- lapply(seq_along(uniqueVals), function(x){
        tempVals <- uniqueVals[[x]]
        nrVals <- length(tempVals)
        if(nrVals < 256){
          nrColours <- nrVals
        } else{
          nrColours <- 256
        }
        
        if(hasColourTable[[x]]){
          breaksTemp <- c(tempVals[1]-1, tempVals)
        } else if(isFactor[[x]]){
          breaksTemp <- c(tempVals[1]-1, raster[[x]]@data@attributes[[1]]$id)
        } else{
          breaksTemp <- c(tempVals[1]-1, seq(tempVals[1], tempVals[[length(tempVals)]], length.out = nrColours))
        }
        valCuts <- cut(vals[[x]], breaks = breaksTemp, include.lowest = TRUE)
        uniqueColours[[x]][valCuts]
      })
    }

    
    # colours for the legend
    tickValues <- lapply(seq_along(uniqueVals), function(x){
      if(length(uniqueVals[[x]]) > theme$bins$legend-1){
        quantile(uniqueVals[[x]], probs = seq(0, 1, length.out = theme$bins$legend), type = 1, names = FALSE)
      } else{
        uniqueVals[[x]]
      }
    })
    tickLabels <- lapply(seq_along(uniqueVals), function(x){
      round(tickValues[[x]], 1)
    })

  } else if(isOpenPlot){
    if(isLegendInPlot){
      legendMeta <- grid.get(gPath("legendTicks"))
      tickLabels <- as.numeric(legendMeta$label)
    }
  } else{
    theme$plot$legend <- FALSE
  }

  # height and width of the plot elements
  if(theme$plot$title){
    titleH <- unit(theme$par$fontsize$title+6, units = "points")
  } else{
    titleH <- unit(0, "points")
  }
  if(theme$plot$legend){
    legendW <- ceiling(convertX(unit(1, "strwidth", as.character(max(unlist(tickLabels)))) + unit(30, "points"), "points"))
  } else{
    legendW <- unit(0, "points")
  }
  if(theme$plot$yAxis){
    yAxisTitleW <- unit(theme$par$fontsize$yAxisTitle+6, units = "points")
    yAxisTicksW <- ceiling(convertX(unit(1, "strwidth", as.character(max(round(axisSteps$y1, theme$bins$yDigits)))), "points"))
  } else{
    yAxisTitleW <- unit(0, "points")
    yAxisTicksW <- unit(0, "points")
  }
  if(theme$plot$xAxis){
    xAxisTitleH <- unit(theme$par$fontsize$xAxisTitle+6, units = "points")
    xAxisTicksH <- unit(theme$par$fontsize$xAxisTicks, units = "points")
  } else{
    xAxisTitleH <- unit(0, "points")
    xAxisTicksH <- unit(0, "points")
  }
  xOffset <- ((as.numeric(yAxisTicksW) + as.numeric(yAxisTitleW)) - as.numeric(legendW))/2

  # determine the number of columns and rows and the position of panels
  if(plotLayers > 1){
    ncol <- ceiling(sqrt(plotLayers))
  } else{
    ncol <- 1
  }
  nrow <- ceiling(plotLayers/ncol)
  panelPosY <- rep(rev(seq(from = 1, to = nrow)), each = ncol)
  panelPosX <- rep(seq(from = 1, to = ncol), times = nrow)

  # create new plot and an overarching viewport
  if(!isOpenPlot){
    grid.newpage()
    pushViewport(viewport(name = "vpLomm"))
  }

  # start plotting the different elements
  for(i in 1:plotLayers){
    plotName <- panelNames[i]

    if(!isOpenPlot){

      # the panel viewport
      pushViewport(viewport(x = (panelPosX[i]/ncol)-(1/ncol/2),
                            y = (panelPosY[i]/nrow)-(1/nrow/2),
                            width = 1/ncol,
                            height = 1/nrow,
                            name = plotName))
      grid.rect(width = convertX(unit(1, "npc"), "native"), gp = gpar(col = NA, fill = NA), name = "panelGrob")
      grid.rect(height = theme$margin$yAxis, width = theme$margin$xAxis,
                gp = gpar(fill = NA, col = NA), name = "marginGrob")
      grid.rect(height = unit(panelExt[[4]] - panelExt[[3]], "points"),
                width = unit(panelExt[[2]] - panelExt[[1]], "points"),
                gp = gpar(fill = NA, col = NA), name = "extentGrob")

      # determine dimensions for this plot
      gridH <- unit(1, "grobheight", "panelGrob") - xAxisTitleH - xAxisTicksH - titleH
      gridHr <- unit(1, "grobwidth", "panelGrob")*ratio$y - yAxisTitleW*ratio$y - yAxisTicksW*ratio$y - legendW*ratio$y
      gridW <- unit(1, "grobwidth", "panelGrob") - yAxisTitleW - yAxisTicksW - legendW
      gridWr <- unit(1, "grobheight", "panelGrob")*ratio$x - xAxisTitleH*ratio$x- xAxisTicksH*ratio$x - titleH*ratio$x

      pushViewport(viewport(x = unit(0.5, "npc") + unit(xOffset, "points"),
                            height = min(gridH, gridHr),
                            width = min(gridW, gridWr),
                            name = "plot"))

      # the title viewport
      if(theme$plot$title){
        pushViewport(viewport(name = "title"))
        grid.text(just = "top",
                  y = unit(1, "npc") + titleH,
                  label = plotName,
                  gp = gpar(fontsize = theme$par$fontsize$title,
                            col = theme$par$colour$title))
        upViewport() # exit title
      }

      # the legend viewport
      if(theme$plot$legend){

        if(length(tickValues[[i]]) < 2){
          pushViewport(viewport(height = unit(1, "npc")*theme$legend$sizeRatio,
                                yscale = c(1, length(uniqueVals[[i]])+0.1),
                                name = "legend"))
        } else{
          pushViewport(viewport(height = unit(1, "npc")*theme$legend$sizeRatio,
                                yscale = c(1, length(uniqueVals[[i]])+0.1),
                                name = "legend"))
        }

        # order the legend
        if(theme$legend$ascending){
          theLegend <- matrix(data = rev(uniqueColours[[i]]), ncol = 1, nrow = length(uniqueColours[[i]]))
          valPos <- unit(which(uniqueVals[[i]] %in% tickValues[[i]]), "native")
        } else{
          theLegend <- matrix(data = uniqueColours[[i]], ncol = 1, nrow = length(uniqueColours[[i]]))
          valPos <- rev(unit(which(uniqueVals[[i]] %in% tickValues[[i]]), "native"))
        }

        grid.raster(x = unit(1, "npc") + unit(10, "points"),
                    width = unit(10, "points"),
                    height = unit(1, "npc"),
                    just = "left",
                    image = theLegend,
                    name = "theLegend",
                    interpolate = FALSE)
        grid.rect(x = unit(1, "npc") + unit(10, "points"),
                  just = "left",
                  width = unit(1, "grobwidth", "theLegend"),
                  gp = gpar(col = "black", fill = NA, alpha = 1, lwd = 0.2))
        grid.text(label = tickLabels[[i]],
                  x = unit(1, "npc") + unit(1, "grobwidth", "theLegend") + unit(20, "points"),
                  y = valPos,
                  just = c("left"),
                  name = "legendTicks",
                  gp = gpar(fontsize = theme$par$fontsize$legend,
                            col = theme$par$colour$legend))


        upViewport() # exit legend
      }

      # the yAxis viewport
      if(theme$plot$yAxis){
        pushViewport(viewport(name = "yAxisTitle"))
        grid.text(just = "right",
                  x = unit(0, "npc") - unit(2, "points") - yAxisTicksW,
                  label = theme$labels$yAxis,
                  rot = theme$par$rotation$yAxisTitle,
                  gp = gpar(fontsize = theme$par$fontsize$yAxisTitle,
                            col = theme$par$colour$yAxisTitle))
        upViewport() # exit yAxisTitle
      }

      # the xAxis viewport
      if(theme$plot$xAxis){
        pushViewport(viewport(name = "xAxisTitle"))
        grid.text(just = "bottom",
                  y = unit(0, "npc") - unit(2, "points") - xAxisTitleH,
                  label = theme$labels$xAxis,
                  rot = theme$par$rotation$xAxisTitle,
                  gp = gpar(fontsize = theme$par$fontsize$xAxisTitle,
                            col = theme$par$colour$xAxisTitle))
        upViewport() # exit xAxisTitle
      }

      # the grid viewport
      pushViewport(viewport(xscale = c(panelExt[[1]]-margin$x, panelExt[[2]]+margin$x),
                            yscale = c(panelExt[[3]]-margin$y, panelExt[[4]]+margin$y),
                            name = "grid"))
      grid.rect(gp = gpar(col = NA, fill = NA), name = "gridGrob")
      # grid.rect(gp = gpar(col = "green", fill = NA))

      if(theme$plot$grid){

        # the grid and axes viewport
        pushViewport(viewport(xscale = c(panelExt[[1]]-margin$x, panelExt[[2]]+margin$x),
                              yscale = c(panelExt[[3]]-margin$y, panelExt[[4]]+margin$y),
                              name = "majorGrid"))

        if(theme$plot$xAxis){
          grid.text(label = as.character(round(axisSteps$x1, theme$bins$xDigits)),
                    just = "top",
                    x = unit(axisSteps$x1, "native"),
                    y = unit(-0.005, "npc"),
                    rot = theme$par$rotation$xAxisTicks,
                    name = "xAxisTicks",
                    gp = gpar(fontsize = theme$par$fontsize$xAxisTicks,
                              col = theme$par$colour$xAxisTicks))
        }
        if(theme$plot$yAxis){
          grid.text(label = as.character(round(axisSteps$y1, theme$bins$yDigits)),
                    just = "right",
                    x = unit(-0.005, "npc"),
                    y = unit(axisSteps$y1, "native"),
                    rot = theme$par$rotation$yAxisTicks,
                    name = "yAxisTicks",
                    gp = gpar(fontsize = theme$par$fontsize$yAxisTicks,
                              col = theme$par$colour$yAxisTicks))
        }

        grid.grill(h = unit(axisSteps$y1, "native"),
                   v = unit(axisSteps$x1, "native"),
                   gp = gpar(lwd = 0.2))
        upViewport() # exit majorGrid

        # plot the minor grid
        if(theme$plot$minorGrid){
          pushViewport(viewport(xscale = c(panelExt[[1]]-margin$x, panelExt[[2]]+margin$x),
                                yscale = c(panelExt[[3]]-margin$y, panelExt[[4]]+margin$y),
                                name = "minorGrid"))
          grid.grill(h = unit(axisSteps$y2, "native"),
                     v = unit(axisSteps$x2, "native"),
                     gp = gpar(lwd = 0.1))
          upViewport() # exit minorGrid
        }
      }

      # the raster viewport
      if(existsGridded){
        pushViewport(viewport(width = unit(1, "npc") - unit(2*margin$x, "native"),
                              height = unit(1, "npc") - unit(2*margin$y, "native"),
                              xscale = c(panelExt[[1]]-margin$x, panelExt[[2]]+margin$x),
                              yscale = c(panelExt[[3]]-margin$y, panelExt[[4]]+margin$y),
                              name = "raster"))
        # grid.rect(gp = gpar(col = "green", fill = NA), name = "rasterGrob")
        grid.raster(width = unit(1, "npc"),
                    height = unit(1, "npc"),
                    image = matrix(data = theColours[[i]], nrow = dims[1], ncol = dims[2], byrow = TRUE),
                    name = "theRaster",
                    interpolate = FALSE)
        upViewport() # exit raster
      }

      # the geom viewport
      if(existsGeom){
        grid.clip()
        pushViewport(viewport(width = unit(1, "npc") - unit(2*margin$x, "native"),
                              height = unit(1, "npc") - unit(2*margin$y, "native"),
                              name = "geom"))
        grid.draw(geomGrob)
        upViewport() # exit geom
      }
      upViewport(3) # exit grid and plot and 'plotName'
    }

    if(isOpenPlot & existsGeom){

      # downViewport("vpLomm")
      downViewport(plotName)
      downViewport("plot")

      if(!isGeomInPlot){
        # this does not seem to shrink the viewport so that geoms are shown correctly.
        pushViewport(viewport(xscale = c(panelExt[[1]]-margin$x, panelExt[[2]]+margin$x),
                              yscale = c(panelExt[[3]]-margin$y, panelExt[[4]]+margin$y),
                              name = "grid"))
        grid.clip()
        pushViewport(viewport(width = unit(1, "npc") - unit(2*margin$x, "native"),
                              height = unit(1, "npc") - unit(2*margin$y, "native"),
                              name = "geom"))
        # grid.rect(gp = gpar(col = NA, fill = NA), name = "gridGrob")
      } else{
        downViewport("grid")
        downViewport("geom")
      }

      grid.draw(geomGrob)
      upViewport(4)
    }

  }

  if(!isOpenPlot){
    upViewport()
  } else if(isOpenPlot & existsGeom){
    upViewport()
  }

  if(trace){
    if(isRasterStackBrick){
      theHistory <- lapply(seq_along(names(raster)), function(x){
        temp <- unlist(raster[[x]]@history)
      })
      if(!is.null(unlist(theHistory))){
        histMsg <- lapply(seq_along(names(raster)), function(x){
          paste0("the layer '", names(raster)[x], "' has the following history:\n -> ", paste0(theHistory[[x]], collapse = "\n -> "))
        })
        names(histMsg) <- names(raster)
        plotHistory <- TRUE
      } else{
        plotHistory <- FALSE
      }
    } else if(isRaster){
      theHistory <- unlist(raster@history)
      if(!is.null(theHistory)){
        histMsg <- paste0("this object has the following history:\n -> ", paste0(theHistory, collapse = "\n -> "))
        plotHistory <- TRUE
      } else{
        plotHistory <- FALSE
      }
    }
    if(plotHistory){
      message(paste0(histMsg, collapse = "\n"))
    } else{
      message(paste0("this object has the following history:\n -> object loaded from memory"))
    }
  }

  invisible(recordPlot(attach = "rasterTools"))
}

#' Create a new theme
#'
#' This is merely a tentative workaround which I will improve with respect to
#' usability in the future. To see the default settings, type
#' \code{str(theme_rt)}.
#' @param from [\code{theme}]\cr the theme that serves as basis for
#'   modifications, by default \code{theme_rt}.
#' @param plot [\code{named list(logical)}]\cr which elements (not) to plot:
#'   \code{title}, \code{legend}, \code{yAxis}, \code{xAxis}, \code{grid},
#'   \code{minorGrid} and \code{commonScale}.
#' @param labels [\code{named list(character)}]\cr the labels of: \code{xAxis}
#'   and \code{yAxis}.
#' @param bins [\code{named list(integerish)}]\cr how many sections for:
#'   \code{yAxis}, \code{xAxis} and \code{legend} and how many \code{yDigits}
#'   and \code{xDigits}.
#' @param margin [\code{named list(numeric)}]\cr the margin proportion for:
#'   \code{yAxis} and \code{xAxis}.
#' @param scale [\code{named list(.)}]\cr at least two \code{colours}, a
#'   \code{bias}, the colour \code{space} and how to \code{interpolate} the
#'   values; see arguments to \code{\link[grDevices]{colorRampPalette}}.
#' @param legend [\code{named list(.)}]\cr whehter or not to sort the legend
#'   values \code{ascending}, the legend \code{position} and the
#'   \code{sizeRatio} of legend:grid.
#' @param fontsize [\code{named list(numeric)}]\cr the fontsize of:
#'   \code{title}, \code{y/xAxisTitle}, \code{y/xAxisTicks} and \code{legend}.
#' @param colour [ \code{named list(character)}]\cr the colour of: \code{title},
#'   \code{y/xAxisTitle}, \code{y/xAxisTicks}, \code{legend} and \code{geom}.
#' @param rotation [\code{named list(numeric)}]\cr the rotation of:
#'   \code{y/xAxisTitle} and \code{y/xAxisTicks}
#' @param fill [\code{named list(character)}]\cr (only \code{geom}) colour for
#'   filling geometries.
#' @param linetype [\code{named list(character)}]\cr (only \code{geom}) line
#'   type; see \code{\link[graphics]{par}}.
#' @param linewidth [\code{named list(numeric)}]\cr (only \code{geom}) line
#'   width; see \code{\link[graphics]{par}}.
#' @param pointsize [\code{named list(numeric)}]\cr (only \code{geom}) the size
#'   of point geometries; see \code{\link[grid]{grid.points}}.
#' @param pointsymbol [\code{named list(integerish)}]\cr (only \code{geom})
#'   point symbol; see \code{\link[graphics]{points}}.
#' @details In case a colourtable is defined in a raster, this overrides the
#'   'scale'.
#' @examples
#' input <- rtData$continuous
#' myTheme <- setTheme(theme_rt,
#'                     plot = list(title = FALSE),
#'                     scale = list(colours = terrain.colors(10), bias = 0.5),
#'                     bins = list(yAxis = 4, xAxis = 5),
#'                     fontsize = list(yAxisTitle = 10, xAxisTicks = 10),
#'                     colour = list(yAxisTitle = "darkgrey", xAxisTitle = "#A9A9A9"),
#'                     rotation = list(yAxisTitle = 0))
#'
#' myTheme <- setTheme(plot = list(title = FALSE))
#' visualise(input, theme = myTheme)
#' @importFrom checkmate assertList assertLogical assertNames
#' @export

setTheme <- function(from = NULL, plot = NULL, labels = NULL, bins = NULL, margin = NULL,
                     scale = NULL, legend = NULL, fontsize = NULL, colour = NULL,
                     rotation = NULL, fill = NULL, linetype = NULL, linewidth = NULL,
                     pointsize = NULL, pointsymbol = NULL){
  
  
  # theme <- list(title <- list(plot = TRUE,
  #                             fontsize = 16,
  #                             colour = "black"),
  #               box <- list(plot  = FALSE,
  #                           linewidth = 3,
  #                           linetype = "solid",
  #                           colour = "black"),
  #               xAxis <- list(plot = TRUE,
  #                             bins = 4,
  #                             margin = 0.05,
  #                             label = list(
  #                               title = "x",
  #                               fontsize = 12,
  #                               colour = "black",
  #                               rotation = 0,
  #                               digits = 1),
  #                             ticks = list(
  #                               fontsize = 10,
  #                               colour = "black")),
  #               yAxis <- list(plot = TRUE,
  #                             bins = 4,
  #                             margin = 0.05,
  #                             label = list(
  #                               title = "x",
  #                               fontsize = 12,
  #                               colour = "black",
  #                               rotation = 0,
  #                               digits = 1),
  #                             ticks = list(
  #                               fontsize = 10,
  #                               colour = "black")),
  #               grid <- list(plot = TRUE,
  #                            minor = TRUE,
  #                            colour = "grey",
  #                            linetype = "solid",
  #                            linewidth = 3),
  #               legend <- list(plot = TRUE,
  #                              common = FALSE,
  #                              bins = 5,
  #                              ascending = TRUE,
  #                              position = "left",
  #                              sizeRatio = 0.6,
  #                              title = list(
  #                                fontsize = 10,
  #                                colour = "black"),
  #                              label = list(
  #                                fontsize = 10,
  #                                colour = "black"),
  #                              ticks = list(
  #                                fontsize = 10,
  #                                colour = "black"),
  #                              box = list(
  #                                linetype = "solid",
  #                                linewidth = 1,
  #                                colour = "black")),
  #              geom <- list(scale = list(x = "fill", to = "id"),
  #                           line = theme_rt$scale$geom$colours,
  #                           fill = theme_rt$scale$geom$colours,
  #                           linetype = "solid",
  #                           linewidth = 3,
  #                           pointsize = 1,
  #                           pointsymbol = 4),
  #               raster <- list(scale = "id",
  #                              colours = theme_rt$scale$raster$colours))
  # 
  # visualise(geom = test, 
  #           theme = setTheme(geom = list(scale = c(x = "line", to = "id"), 
  #                                        fill = NA, 
  #                                        line = c('#FFFFFF', '#000000')))))
  
  assertList(from, len = 7, null.ok = TRUE)
  if(!is.null(from)){
    assertNames(names(from), permutation.of = c("plot", "labels", "bins", "margin", "scale", 
                                                "legend", "par"))
  } else{
    from <- theme_rt
  }
  out <- from
  assertList(plot, any.missing = FALSE, types = c("logical"), null.ok = TRUE)
  if(!is.null(plot)){
    assertNames(names(plot), subset.of = c("title", "legend", "yAxis", "xAxis", "grid", "minorGrid", "commonScale"))
    prvPlot <- from[which(names(from) == "plot")]$plot
    matched <- names(prvPlot) %in% names(plot)
    out$plot[matched] <- plot
  }
  
  assertList(labels, any.missing = FALSE, types = c("character"), null.ok = TRUE)
  if(!is.null(labels)){
    assertNames(names(labels), subset.of = c("yAxis", "xAxis"))
    prvBins <- from[which(names(from) == "labels")]$labels
    matched <- names(prvBins) %in% names(labels)
    out$labels[matched] <- labels
  }
  
  assertList(bins, any.missing = FALSE, types = c("integerish"), null.ok = TRUE)
  if(!is.null(bins)){
    assertNames(names(bins), subset.of = c("yAxis", "xAxis", "legend", "yDigits", "xDigits"))
    prvBins <- from[which(names(from) == "bins")]$bins
    matched <- names(prvBins) %in% names(bins)
    out$bins[matched] <- bins
  }
  
  assertList(margin, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(margin)){
    assertNames(names(margin), subset.of = c("yAxis", "xAxis"))
    assertNumeric(margin$yAxis, lower = 0, upper = 1, null.ok = TRUE)
    assertNumeric(margin$xAxis, lower = 0, upper = 1, null.ok = TRUE)
    prvMargin <- from[which(names(from) == "margin")]$margin
    matched <- names(prvMargin) %in% names(margin)
    out$margin[matched] <- margin
  }
  
  assertList(scale, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(scale$raster)){
    assertNames(names(scale), subset.of = c("raster", "geom"))
    assertNames(names(scale$raster), subset.of = c("colours", "variable"))
    assertCharacter(scale$raster$colours, null.ok = TRUE)
    assertCharacter(scale$raster$variable, null.ok = TRUE)
    
    prvScale <- from[which(names(from) == "scale")]$scale$raster
    matched <- names(prvScale) %in% names(scale$raster)
    out$scale$raster[matched] <- scale$raster
  }
  if(!is.null(scale$geom)){
    assertNames(names(scale), subset.of = c("raster", "geom"))
    assertNames(names(scale$geom), subset.of = c("colours", "variable"))
    assertCharacter(scale$geom$colours, null.ok = TRUE)
    assertCharacter(scale$geom$variable, null.ok = TRUE)
    
    prvScale <- from[which(names(from) == "scale")]$scale$geom
    matched <- names(prvScale) %in% names(scale$geom)
    out$scale$geom[matched] <- scale$geom
  }
  
  assertList(legend, any.missing = FALSE, null.ok = TRUE)
  if(!is.null(legend)){
    assertNames(names(legend), subset.of = c("ascending", "position", "sizeRatio"))
    assertLogical(legend$ascending, null.ok = TRUE)
    assertLogical(legend$interpolate, null.ok = TRUE)
    assertCharacter(legend$position, null.ok = TRUE)
    assertNumeric(legend$ratio, null.ok = TRUE)
    prvScale <- from[which(names(from) == "legend")]$legend
    matched <- names(prvScale) %in% names(legend)
    out$legend[matched] <- legend
  }
  
  assertList(fontsize, any.missing = FALSE, types = c("numeric"), null.ok = TRUE)
  if(!is.null(fontsize)){
    assertNames(names(fontsize), subset.of = c("title", "yAxisTitle", "yAxisTicks", "xAxisTitle", "xAxisTicks", "legend"))
    prvFontsize <- from$par[which(names(from$par) == "fontsize")]$fontsize
    matched <- names(prvFontsize) %in% names(fontsize)
    out$par$fontsize[matched] <- fontsize
  }
  
  assertList(colour, any.missing = FALSE, types = c("character"), null.ok = TRUE)
  if(!is.null(colour)){
    assertNames(names(colour), subset.of = c("title", "yAxisTitle", "yAxisTicks", "xAxisTitle", "xAxisTicks", "legend", "geom"))
    prvColour <- from$par[which(names(from$par) == "colour")]$colour
    matched <- names(prvColour) %in% names(colour)
    out$par$colour[matched] <- colour
  }
  
  assertList(rotation, any.missing = FALSE, types = c("numeric"),null.ok = TRUE)
  if(!is.null(rotation)){
    assertNames(names(rotation), subset.of = c("yAxisTitle", "yAxisTicks", "xAxisTitle", "xAxisTicks"))
    prvPRotation <- from$par[which(names(from$par) == "rotation")]$rotation
    matched <- names(prvPRotation) %in% names(rotation)
    out$par$rotation[matched] <- rotation
  }
  
  assertList(fill, any.missing = FALSE, types = c("character"), null.ok = TRUE)
  if(!is.null(fill)){
    assertNames(names(fill), subset.of = c("geom"))
    prvFill <- from$par[which(names(from$par) == "fill")]$fill
    matched <- names(prvFill) %in% names(fill)
    out$par$fill[matched] <- fill
  }
  
  assertList(linetype, any.missing = FALSE, types = c("character"), null.ok = TRUE)
  if(!is.null(linetype)){
    assertNames(names(linetype), subset.of = c("geom"))
    prvLinetype <- from$par[which(names(from$par) == "linetype")]$linetype
    matched <- names(prvLinetype) %in% names(linetype)
    out$par$linetype[matched] <- linetype
  }
  
  assertList(linewidth, any.missing = FALSE, types = c("numeric"), null.ok = TRUE)
  if(!is.null(linewidth)){
    assertNames(names(linewidth), subset.of = c("geom"))
    prvLinewidth <- from$par[which(names(from$par) == "linewidth")]$linewidth
    matched <- names(prvLinewidth) %in% names(linewidth)
    out$par$linewidth[matched] <- linewidth
  }
  
  assertList(pointsize, any.missing = FALSE, types = c("numeric"), null.ok = TRUE)
  if(!is.null(pointsize)){
    assertNames(names(pointsize), subset.of = c("geom"))
    prvPointsize <- from$par[which(names(from$par) == "pointsize")]$pointsize
    matched <- names(prvPointsize) %in% names(pointsize)
    out$par$pointsize[matched] <- pointsize
  }
  
  assertList(pointsymbol, any.missing = FALSE, types = c("integerish"), null.ok = TRUE)
  if(!is.null(pointsymbol)){
    assertNames(names(pointsymbol), subset.of = c("geom"))
    prvPointsymbol <- from$par[which(names(from$par) == "pointsymbol")]$pointsymbol
    matched <- names(prvPointsymbol) %in% names(pointsymbol)
    out$par$pointsymbol[matched] <- pointsymbol
  }

  return(out)
}
