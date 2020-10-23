#' Find days elapsed between two trips 
#' 
#' @param userid User ID of interest.
#' 
#' @return The number of trips that were taken by \code{userid} that were on the weekend.
#' 
#' @examples
#' userid("478b8a07-cb2d-419f-8ea6-c4c14132bb92")
#' @export
num_weekend_trips <- function(userid) {

  user_trips <- filter(trips, user_id == userid)
    
  weekend <- purrr::map_int(.x = user_trips$start_time, .f = weekend_or_holiday)
  
  return(sum(weekend))
}
