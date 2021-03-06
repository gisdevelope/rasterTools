#' Obtain Global Forest Change data
#'
#' Obtain data from the 'Global Forest Change'
#' \href{https://earthenginepartners.appspot.com/science-2013-global-forest}{dataset}
#' (\href{https://doi.org/10.1126/science.1244693}{paper}).
#'
#' @template mask
#' @param years [\code{integerish(.)}]\cr year(s) for which GFC data should be
#'   extracted; see Details.
#' @param keepRaw [\code{logical(1)}]\cr should the raw data be retained
#'   (\code{TRUE}), or should only the derived data be returned (\code{FALSE},
#'   default)?
#' @details The GFC dataset is based on a time-series analysis of Landsat images
#'   characterizing forest extent and change.
#'
#'   A problem with the GFC dataset is that the gain-layer is calculated for the
#'   overall period from 2000 to 2014, while the loss-layer contains the loss
#'   events on a yearly basis. Hence, to find the true value per year for a
#'   raster-cell may not be straightforward. In \code{oGFC} a yearly value is
#'   derived by removing all loss events up to the year in question from the
#'   \emph{year 2000}-layer and subsequently adding the \emph{gain}-layer. Gain
#'   events are in nature rather diffuse and happen progressively and relatively
#'   slowely throughout time. A raster cell, which was marked as "forest absent"
#'   in 2000 and which had a positive gain-value by 2014, "became tree" within
#'   this time-frame (i.e. became over 5 m tall). This event, "becoming tree",
#'   presumably does not mean that the vegetation in this raster cell grew to a
#'   tall and mature forest. Much rather it would have some hight between 5 m
#'   and what a tree in the given region can grow in that short time. To get a
#'   more accurate estimation of the forest cover - particularly in very dynamic
#'   landscapes - it might be wise to weigh the forest cover with some sort of
#'   productivity and/or macroclimate dataset, because more suitable sites
#'   result in faster growth of trees.
#' @return A \code{RasterStack} of gfc data.
#' @references  Hansen, M.C., Potapov, P.V., Moore, R., Hancher, M., Turubanova,
#'   S.A., Tyukavina, A., Thau, D., Stehman, S.V., Goetz, S.J., Loveland, T.R.,
#'   Kommareddy, A., Egorov, A., Chini, L., Justice, C.O., Townshend, J.R.G.,
#'   2013. High-Resolution Global Maps of 21st-Century Forest Cover Change.
#'   Science 342, 846–850.
#' @family obtain operators (Global)
#' @examples
#' \dontrun{
#'
#' require(magrittr)
#'
#' myGFC <- oGFC(mask = rtGeoms$mask,
#'               years = c(2002, 2006, 2010, 2014))
#' visualise(raster = myGFC$treecover_2002, trace = TRUE)
#'
#' # get the (updated) bibliography
#' reference(style = "bibtex")
#' 
#' # the gfc tiles
#' gfcWindow <- data.frame(x = c(-180, 180),
#'                         y = c(-60, 80))
#' tiles_gfc <- geomTiles(window = gfcWindow, cells = c(36, 14), 
#'                        crs = projs$longlat)
#' visualise(geom = tiles_gfc)
#' }
#' @importFrom stringr str_split
#' @importFrom raster crop mosaic projectRaster stack
#' @export

oGFC <- function(mask = NULL, years = NULL, keepRaw = FALSE){
  # check whether a smaller window also works, after determining new coordinates

  # check arguments
  maskIsGeom <- testClass(mask, classes = "geom")
  maskIsSp <- testClass(mask, classes = "Spatial")
  maskIsSf <- testClass(mask, classes = "sf")
  assert(maskIsGeom, maskIsSp, maskIsSf)
  assertIntegerish(years, any.missing = FALSE, min.len = 1)
  assertTRUE(all(years %in% c(2000:2014)))
  assertLogical(keepRaw)

  cols <- c("#EAF3E6", "#E8F1E3", "#E6F0E1", "#E4EEDF", "#E3EDDD", "#E1ECDB",
            "#DFEAD9", "#DDE9D7", "#DBE8D4", "#DAE6D2", "#D8E5D0", "#D6E3CE",
            "#D4E2CC", "#D3E1CA", "#D1DFC8", "#CFDEC6", "#CEDDC3", "#CCDBC1",
            "#CADABF", "#C8D8BD", "#C6D7BB", "#C5D6B9", "#C3D4B7", "#C1D3B5",
            "#C0D2B2", "#BED0B0", "#BCCFAE", "#BACEAC", "#B9CCAA", "#B7CBA8",
            "#B5C9A6", "#B3C8A3", "#B2C7A1", "#B0C59F", "#AEC49D", "#ACC39B",
            "#ABC199", "#A9C097", "#A7BE95", "#A5BD92", "#A3BC90", "#A2BA8E",
            "#A0B98C", "#9EB88A", "#9DB688", "#9BB586", "#99B384", "#97B281",
            "#95B17F", "#94AF7D", "#92AE7B", "#90AD79", "#8FAB77", "#8DAA75",
            "#8BA972", "#89A770", "#87A66E", "#86A46C", "#84A36A", "#82A268",
            "#81A066", "#7F9F64", "#7D9E61", "#7B9C5F", "#799B5D", "#78995B",
            "#769859", "#749757", "#729555", "#719453", "#6F9350", "#6D914E",
            "#6C904C", "#6A8E4A", "#688D48", "#668C46", "#648A44", "#638941",
            "#61883F", "#5F863D", "#5D853B", "#5C8439", "#5A8237", "#588135",
            "#577F33", "#557E30", "#537D2E", "#517B2C", "#507A2A", "#4E7928",
            "#4C7726", "#4A7624", "#497422", "#47731F", "#45721D", "#43701B",
            "#416F19", "#406E17", "#3E6C15", "#3C6B13", "#3B6A11", rep("#000000", 155))

  targetCRS <- getCRS(x = mask)
  maskExtent <- getExtent(x = mask)
  if(targetCRS != projs$longlat){
    targetMask <- setCRS(x = mask, crs = projs$longlat)
  } else{
    targetMask <- mask
  } 
  maskGeom <- geomRectangle(anchor = getExtent(x = targetMask))
  maskGeom <- setCRS(x = maskGeom, crs = projs$longlat)
  targetExtent <- getExtent(maskGeom)
  
  # create the tiles geometry to determine the data-subset to load
  gfcWindow <- data.frame(x = c(-180, 180),
                          y = c(-60, 80))
  tiles_gfc <- geomTiles(window = gfcWindow, cells = c(36, 14), crs = projs$longlat)

  # determine tiles of interest
  tabGFC <- getCoords(x = tiles_gfc)
  tabMask <- getCoords(x = targetMask)
  ids <- unique(tabGFC$fid)
  xMatch <- yMatch <- NULL
  for(i in seq_along(ids)){
    temp <- tabGFC[tabGFC$fid == ids[i],]
    xMatch <- c(xMatch, ifelse(any(tabMask$x < max(temp$x)) & any(tabMask$x > min(temp$x)), TRUE, FALSE))
    yMatch <- c(yMatch, ifelse(any(tabMask$y < max(temp$y)) & any(tabMask$y > min(temp$y)), TRUE, FALSE))
  }
  tiles <- xMatch & yMatch
  myTiles <- getSubset(x = tiles_gfc, attr = tiles)

  layerNames <- c("treecover2000", "loss", "gain", "lossyear", "datamask")
  allObjects <- NULL

  tabTiles <- getCoords(x = myTiles)
  # go through all selected tiles and subset them with the mask
  history <- list()
  tempOut <- list()
  for (i in unique(tabTiles$fid)){
    min_x <- min(tabTiles$x[tabTiles$fid == i])
    max_y <- max(tabTiles$y[tabTiles$fid == i])
    
    if(min_x < 0){
      easting <- paste0(sprintf('%03i', abs(min_x)), 'W')
    } else{
      easting <-  paste0(sprintf('%03i', min_x), 'E')
    }
    if(max_y < 0){
      northing <- paste0(sprintf('%02i', abs(max_y)), 'S')
    } else{
      northing <- paste0(sprintf('%02i', max_y), 'N')
    }
    gridId <- paste0(northing, '_', easting)
    fileNames <-  paste0("Hansen_GFC2015_", layerNames, "_", gridId, '.tif')
    fileExists <- sapply(seq_along(fileNames), function(x){
      testFileExists(paste0(rtPaths$gfc$local, "/", fileNames[x]))
    })
    message(paste0("I am handling the gfc datasets with the grid ID '", gridId, "' ..."))
    
    if(any(!fileExists)){
      downloadGFC(file = fileNames[!fileExists],
                  localPath = rtPaths$gfc$local)
    }
    
    tempObject <- stack(
      lapply(seq_along(fileNames), function(x){
        temp <- strsplit(fileNames[x], "_")[[1]]
        shortName <- paste0(temp[2], "_", temp[3], "_", gridId)
        gdalwarp(srcfile = paste0(rtPaths$gfc$local, "/", fileNames[x]),
                 dstfile = paste0(rtPaths$project, "/", shortName, "_", paste0(round(maskExtent$x), collapse = "."), "_", paste0(round(maskExtent$y), collapse = "."), ".tif"),
                 s_srs = projs$longlat,
                 t_srs = targetCRS,
                 te = c(maskExtent$x[1], maskExtent$y[1], maskExtent$x[2], maskExtent$y[2]),
                 overwrite = TRUE,
                 output_Raster = TRUE)
      })
    )
    
    history <- c(history, paste0("object loaded with the grid ID '", gridId, "'"))
    history <-  c(history, paste0("object cropped between points (x, y) '", targetExtent$x[1], ", ", targetExtent$y[1], "' and '", targetExtent$x[2], ", ", targetExtent$y[2], "'"))
    if(targetCRS != projs$longlat){
      crs_name <- strsplit(targetCRS, " ")[[1]][1]
      history <- c(history, list(paste0("object reprojected to ", crs_name)))
    }
    
    fc00 <- tempObject[[1]]
    fc00[tempObject[[5]]==2] <- NA
    gain <- tempObject[[3]]
    loss <- tempObject[[4]]
    
    for(j in seq_along(years)){
      theName <- paste0("treecover_", years[j])
      loss_temp <- loss<years[j]-2000 & loss>0
      temp <- fc00
      temp[loss_temp > 0] <- 0
      names(temp) <- theName
      history <- c(history, list(paste0(theName, " has been calculated as 'treecover2000 - loss_by_", years[j], "'")))
      tempObject <- stack(tempObject, temp)
    }
    
    if(!keepRaw){
      tempObject <- tempObject[[-c(1:5),]]
      if(dim(tempObject)[3] == 1){
        tempObject@legend@colortable <- cols
      } else{
        for(i in 1:dim(tempObject)[3]){
          tempObject[[i]]@legend@colortable <- cols
        }
      }
    } else{
      for(i in 6:dim(tempObject)[3]){
        tempObject[[i]]@legend@colortable <- cols
      }
    }
    tempOut <- c(tempOut, tempObject)
  }
  
  # merge, if there are several tiles involved
  if(length(tempOut) > 1){
    message(" ... merging two tiles")
    tempOut <- mosaic_rasters(gdalfile = unlist(lapply(tempOut, function(x) x[[1]]@file@name)),
                              dst_dataset = ,
                              overwrite = TRUE,
                              output_Raster = TRUE)
    history <- c(history, paste0("tiles merged as mosaic"))
    file.remove()
  } else{
    tempOut <- tempOut[[1]]
  }
  # if(length(allObjects) > 1){
  #   allObjects$fun <- "mean"
  #   gfc_out <- do.call(mosaic, allObjects)
  #   history <- do.call(rbind, history)
  #   # this probably needs updating, the histories need to be merged in a sensible way.
  # } else{
  #   gfc_out <- allObjects[[1]]
  # }

  tempOut@history <- history

  # manage the bibliography entry
  bib <- bibentry(bibtype = "Article",
                  title = "High-Resolution Global Maps of 21st-Century Forest Cover Change",
                  author = c(person(given = "M C", family = "Hansen"),
                             person(given = "P V", family = "Potapov"),
                             person(given = "R", family = "Moore"),
                             person(given = "M", family = "Hancher"),
                             person(given = "S A", family = "Turubanova"),
                             person(given = "A", family = "Tyukavina"),
                             person(given = "D", family = "Thau"),
                             person(given = "S V", family = "Stehman"),
                             person(given = "S J", family = "Goetz"),
                             person(given = "T R", family = "Loveland"),
                             person(given = "A", family = "Kommareddy"),
                             person(given = "A", family = "Egorov"),
                             person(given = "L", family = "Chini"),
                             person(given = "C O", family = "Justice"),
                             person(given = "J R G", family = "Townshend")),
                  journal = "Science",
                  number = "6160",
                  volume = "342",
                  year = "2013",
                  pages = "846-850",
                  doi = "10.1126/science.1244693"
  )

  if(is.null(getOption("bibliography"))){
    options(bibliography = bib)
  } else{
    currentBib <- getOption("bibliography")
    if(!bib%in%currentBib){
      options(bibliography = c(currentBib, bib))
    }
  }

  return(tempOut)

}

#' @describeIn oGFC function to download data related to the GFC dataset
#' @param file [\code{character(1)}]\cr the name of the file to download.
#' @template localPath
#' @importFrom httr GET write_disk progress
#' @export

downloadGFC <- function(file = NULL, localPath = NULL){

  assertCharacter(file, any.missing = FALSE, len = 1)
  if(!is.null(localPath)){
    assertDirectory(localPath, access = "rw")
  }

  onlinePath <- rtPaths$gfc$remote
  blablabla(paste0("  ... downloading the file from '", onlinePath, "'"))
  
  GET(url = paste0(onlinePath, file),
      write_disk(paste0(localPath, "/", file), overwrite = TRUE),
      progress())
}