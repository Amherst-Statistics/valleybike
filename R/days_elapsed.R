#' Find days elapsed between two trips 
#' 
#' @param trip1 Date of the first trip.
#' @param trip2 Date of the second trip.
#' 
#' @return The days elapsed of of \code{trip1} and \code{trip2}.
#' 
#' @examples
#' days_elapsed("2019-07-01", "2019-07-30")
#' 

library(tidyverse)
library(valleybikeData)
library(lubridate)

days_elapsed <- function(trip1, trip2){
  
  if(year(trip1) == year(trip2)){
    return(ceiling(difftime(time1 = date(trip2), time2 = date(trip1), units = "days")))
  }else
    return((ceiling(difftime(time1 = date(trip2), time2 = date(trip1), units = "days")))- 122)
  
}