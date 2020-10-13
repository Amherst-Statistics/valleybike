#' Get duration of trip
#'
#' Get the duration of some trip, based on its route ID.
#'
#' @title get_duration
#'
#' @param route_id The route ID (as a string)
#'
#' @return The duration of the route ID's corresponding trip (as a numeric constant)
#'
#' @examples
#' get_duration("route_04_2019@@01253ad9-cfb8-44a5-9218-bff8109e4880")
#'
#' @export
get_duration <- function(route_id) {

  data("trips", envir = environment())

  duration <- as.numeric(trips[trips$route_id == route_id, "duration"])

  if (length(duration) == 0) {
    stop("No trip with that route ID was found. Try again with a valid route ID.")
  }

  return(duration)
}
