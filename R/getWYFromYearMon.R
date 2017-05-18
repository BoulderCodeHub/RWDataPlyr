
#' Get the water year from a month-year (yearmon) value
#' 
#' \code{getWYFromYearmon} returns the water year (assumed to be October - September)
#' from a yearmon object. October - December of a year, are part of the next water
#' year and that will be returned.
#' 
#' If the argument is not already a yearmon object, it will attempt to convert it.
#' This may results in unexpected results. For example, the string \code{"12-1-1906"} can
#' be converted to a yearmon, however, it will not convert to \code{"Dec 1906"} as 
#' you might desire. It will convert to \code{"Jan 0012"} since it is not a format expected
#' by \code{zoo::as.yearmon}.
#' 
#' @param ym An object of class yearmon, or something that can be converted to yearmon
#' @return The water year as a numeric
#' @examples 
#' library(zoo)
#' getWYFromYearmon(as.yearmon(c("Dec 1906", "Oct 1945", "Jul 1955")))
#' getWYFromYearmon("2000-11")
#' 
#' @export

getWYFromYearmon <- function(ym)
{
  if(!(class(ym) == "yearmon")) {
    warning("ym, is not a yearmon object. attempting to convert to yearmon...")
    ym <- zoo::as.yearmon(ym)
    if(is.null(ym)) 
      stop("could not convert ym to yearmon")
  }
  
  mm <- as.numeric(format(ym, '%m'))
  yy <- as.numeric(format(ym, '%Y'))
  # if OND then increase year by one for water year, else leave it the same
  yy[mm > 9] <- yy[mm > 9] + 1
  
  yy
}