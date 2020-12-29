#' Get trajectory data
#'
#' Get the point-by-point trajectory data for a trip.
#'
#' @title get_trajectory_data
#'
#' @param route_id A trip's route ID (as a string).
#'
#' @return A data frame of trajectory data for the given route ID.
#'
#' @examples
#' \dontrun{
#' get_trajectory_data("route_07_2018@44bde968-5454-4db2-a4b1-a32493186f82")
#' }
#'
#' @export
get_trajectory_data <- function(route_id) {

  if (!(route_id %in% valleybikeData::trips$route_id)) {
    stop("Invalid route ID \"", route_id, "\".")
  }

  trip_month <- substr(route_id, start = 7, stop = 8)
  trip_year <- substr(route_id, start = 10, stop = 13)

  data <- valleybikeData::get_monthly_dataset(month = trip_month, year = trip_year) %>%
    dplyr::filter(route_id == !!route_id)

  return(data)
}
