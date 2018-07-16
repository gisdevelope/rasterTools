#' List arguments of the parent function
#'
#' Determine the name and value of the arguments called in a function.
#' @return The output of \code{\link{match.call}} for the parent frame.
#' @examples
#'
#' testFun <- function(x = "character", y = NULL, z = c(1, 2, 3)){
#'   g <- listArgs()
#'   return(g)
#' }
#' testFun(x = "hello world")
#' @export

listArgs <- function (){
  as.list(
    match.call(
      definition = sys.function( -1 ),
      call = sys.call( -1 )
    )
  )[-1]
}

#' Determine depth of a list
#'
#' @param list [\code{list(.)}] list to test.
#' @return an integer value of the depth of a list.
#' @examples
#' x <- list(int = c(1:5),
#'           char = list(lower = c(letters[1:5]),
#'                       upper = c(LETTERS[1:5])))
#' depthList(x)
#' depthList(x[[1]])
#' @export

depthList <- function(list) {
  ifelse(is.list(list), 1L + max(sapply(list, depthList)), 0L)
}

#' Translate 'day of the year' to a date
#'
#' @param year [\code{integerish(1)}]\cr a year value as part of the date.
#' @param doy [\code{integerish(1)}]\cr the day of the year for which you want
#'   the date
#' @return An object of class \code{\link{POSIXct}}.
#' @examples
#' aDate <- doyToDate(year = 2000, doy = 055)
#' @export

doyToDate <- function(year, doy){

  if(!is.numeric(year)) year <- as.numeric(year)
  if(!is.numeric(doy)) doy <- as.numeric(doy)
  if(length(year) != length(doy)) year <- rep_len(year, length(doy))

  leap_year <- seq(from = 1904, to = 2196, by = 4)
  theMonths <- NULL
  theDays <- NULL

  for(i in seq_along(doy)){
    if(year[i] %in% leap_year){
      daysPerMonth <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    } else{
      daysPerMonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }
    days <- c(0, cumsum(daysPerMonth))

    month <- max(which(doy[i] > days))
    theMonths <- c(theMonths, month)

    day <- doy[i] - days[month]
    theDays <- c(theDays, day)
  }

  return(ISOdate(year, theMonths, theDays, hour = 0, tz = "GMT"))
}

#' Scale values
#'
#' Scale the values of a matrix to a specific range
#' @param mat [\code{matrix(1)}]\cr the matrix, in which values shall be scaled
#'   to \code{range}.
#' @param range [\code{integerish(2)}]\cr the range, to which values in
#'   \code{mat} should be scaled to.
#' @return a matrix of scaled values
#' @export

scaleVals <- function(mat, range){

  if(length(range) != 2) stop("please provide a range of two valid values.")
  vals <- unique(as.vector(mat))

  # scale to range
  if(length(vals) != 1){
    out <- (mat - min(mat, na.rm = TRUE)) * (range[2] - range[1]) / (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) + range[1]
  } else{
    message("'mat' did not contain at least two distinct values, hence I return the original matrix.")
    out <- mat
  }

  return(out)
}

#' Transform degree to radians
#' @param degree [\code{numeric(1)}]\cr the degree value to transform.
#' @return a radians value
#' @export

rad <- function(degree){
  assertNumeric(degree)
  (degree * pi)/180
}
