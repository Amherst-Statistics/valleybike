#' Find top station from trips
#' 
#' @param trip_ids Vector of Trip Ids
#' 
#' @return Dataframe containing top station.
#'
#' @export
top_station_from_trips <- function(trip_ids){
  
  data("trips", envir = environment())
  
  user_trip <- trips %>%
    filter(route_id %in% trip_ids)
  
  return(as.data.frame(plyr::count(as.vector(as.matrix(user_trip[, 
                                                                 c("start_station", "end_station")])))) %>%
           arrange(desc(freq)) %>%
           head(1))
  
}