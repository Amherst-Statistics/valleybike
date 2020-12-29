#' Get trip duration
#'
#' Get the duration of a trip.
#'
#' @title get_duration
#'
#' @param route_id A trip's route ID (as a string).
#' @param unit The measurement unit for the duration (one of: "seconds", "minutes", or "hours").
#'             Defaults to "minutes".
#'
#' @return The approximate duration of the trip (in the measurement unit specified).
#'
#' @examples
#' \dontrun{
#' get_duration("route_04_2019@01253ad9-cfb8-44a5-9218-bff8109e4880")
#' }
#'
#' @export
get_duration <- function(route_id, unit = "minutes") {

  if (!(route_id %in% valleybikeData::trips$route_id)) {
    stop("Invalid route ID \"", route_id, "\".")
  }

  if (!(unit %in% c("seconds", "minutes", "hours"))) {
    stop("Unsupported measurement unit \"", unit,
         "\". Must be one of: \"seconds\", \"minutes\", \"hours\".")
  }

  duration_seconds <- valleybikeData::trips %>%
    dplyr::filter(route_id == !!route_id) %>%
    dplyr::pull(duration)

  duration <- switch(
    unit,
    "seconds" = duration_seconds,
    "minutes" = duration_seconds / 60,
    "hours" = duration_seconds / 3600
  )

  return(duration)
}
