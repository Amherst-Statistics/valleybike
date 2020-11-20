#' Get trip data
#' 
#' @param trip_ids Vector of Trip IDs

#' @return Dataframe containing lat/lon coordinates for specified trips.
#' 
#' @examples
#' get_trips ("route_06_2018@20dd04ae-8a3e-4ff1-a078-2e2bfe8c5be46a03818a-b847-4ee7-a8fd-9f394f5d2769")
#'
#' @export
get_trips <- function(trip_ids) {
  
  data("trips", envir = environment())
  
  ds <- trips %>%
    filter(route_id %in% trip_ids)
  
  ds <- ds %>% 
    mutate(month = stringr::str_to_lower(lubridate::month(start_time, label =TRUE, abbr = FALSE)),
           year = lubridate::year(start_time),
           time = paste0(month,year))
  
  df<- map_df(.x = unique(ds$time), function(.x){
    
    return(data.frame(eval(as.name(.x)) %>% filter(route_id %in% ds$route_id )))
  } )
  
  return(df)
}