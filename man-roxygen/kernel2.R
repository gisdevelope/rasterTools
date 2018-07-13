#' @param kernel [\code{matrix(., .)} | \code{list(.)} thereof]\cr scan the
#'   raster with this kernel (default is a 3 by 3 cells diamond kernel).
#' @details The cells of a kernel can have the values 0, 1 and NA. The kernel
#'   cells with values 0 and 1 are matched accurately in the input raster. The
#'   kernel cells with value NA will be ignored.
