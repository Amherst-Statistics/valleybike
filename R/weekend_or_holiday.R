#' Determines i
#' 
#' @param d Date of the trip.
#' 
#' @return TRUE or FALSE depending on if \code{d} is a weekend or holiday
#' 
#' @examples
#' weekend_or_holiday("2019-07-01")
#' 
#' 
library(tidyverse)
library(lubridate)

weekend_or_holiday <- function(d) {
  
  
  holidays <- lubridate::ymd(c("2018-07-04", "2018-09-03", "2018-10-08", "2018-11-22",
                               "2019-05-25", "2019-07-04", "2019-09-07", "2019-11-26", "2019-10-12"))
  
  
  if(wday(d, label = TRUE) == "Sat" || wday(d, label = TRUE) == "Sun" || d %in% holidays ){
    return(TRUE)
    
  } else
    return(FALSE)
}