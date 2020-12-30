#' Get trip distance
#'
#' Get the distance covered by a trip.
#'
#' @title get_distance
#'
#' @param route_id A trip's route ID (as a string).
#' @param unit The measurement unit for the distance (one of: "meters", "kilometers",
#'             "feet", or "miles"). Defaults to "meters".
#'
#' @return The approximate distance covered by the trip (in the measurement unit specified).
#'
#' @examples
#' \dontrun{
#' get_distance("route_04_2019@01253ad9-cfb8-44a5-9218-bff8109e4880")
#' }
#'
#' @export
get_distance <- function(route_id, unit = "meters") {

  if (!(unit %in% c("meters", "kilometers", "feet", "miles"))) {
    stop("Unsupported measurement unit \"", unit,
         "\". Must be one of: \"meters\", \"kilometers\", \"feet\", \"miles\".")
  }

  trajectory_data <- get_trajectory_data(route_id)

  coord_columns <- c("latitude", "longitude")

  before <- trajectory_data %>%
    utils::head(-1) %>%
    dplyr::select(coord_columns) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(x = list(c(latitude, longitude))) %>%
    dplyr::pull(x)

  after <- trajectory_data %>%
    utils::tail(-1) %>%
    dplyr::select(coord_columns) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(y = list(c(latitude, longitude))) %>%
    dplyr::pull(y)

  pairwise_distances <- mapply(geosphere::distVincentyEllipsoid, before, after)

  total_distance_meters <- sum(pairwise_distances)

  total_distance <- switch(
    unit,
    "meters" = total_distance_meters,
    "kilometers" = total_distance_meters / 1000,
    "feet" = total_distance_meters * 3.28084,
    "miles" = (total_distance_meters / 1000) * 0.621371
  )

  return(total_distance)
}
