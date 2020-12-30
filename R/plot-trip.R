#' Plot trip
#'
#' Plot the trajectory of a trip on the map.
#'
#' @title plot_trip
#'
#' @param route_id A trip's route ID (as a string).
#' @param api_key A Google Maps API key to pass to \code{ggmap::register_google} (required).
#' @param layers Any additional ggplot2 layers to add to the plot, such as titles, themes, etc.
#' @param ... Arguments to pass to \code{ggmap::get_map} for customizing the map background,
#'            such as \code{zoom}, \code{maptype}, \code{source}, \code{color}, etc.
#'
#' @return A ggplot2 plot object of the trip's trajectory on the map.
#'
#' @examples
#' \dontrun{
#' plot_trip("route_04_2019@01253ad9-cfb8-44a5-9218-bff8109e4880", zoom = 16,
#'           layers = ggplot2::ggtitle("Trajectory of 01253ad9-cfb8-44a5-9218-bff8109e4880"))
#' }
#'
#' @export
plot_trip <- function(route_id, api_key, layers, ...) {

  if (missing(route_id)) {
    stop("A route ID is needed for plotting trips.")
  }

  if (missing(api_key) && !ggmap::has_google_key()) {
    stop("No Google API key detected. Please provide a valid API key.")
  } else if (!missing(api_key)) {
    ggmap::register_google(api_key)
  }

  route_data <- get_trajectory_data(route_id) %>%
    dplyr::mutate(prop_trip_completed = dplyr::row_number() / nrow(.))

  sides <- max(0.1 * diff(range(route_data$longitude)),
               0.1 * diff(range(route_data$latitude)))

  plotting_boundary <- c(
    min(route_data$longitude, na.rm = T) - sides,
    min(route_data$latitude, na.rm = T) - sides,
    max(route_data$longitude, na.rm = T) + sides,
    max(route_data$latitude, na.rm = T) + sides
  )

  base_map <- ggmap::get_map(location = plotting_boundary, ...)

  emojifont::load.fontawesome()

  map <- ggmap::ggmap(
    base_map,
    base_layer = ggplot2::ggplot(route_data, ggplot2::aes(
      x = longitude,
      y = latitude,
      color = prop_trip_completed
    ))
  ) +
    ggplot2::geom_path(size = 0.85) +
    ggplot2::geom_text(
      label = emojifont::fontawesome("fa-flag-checkered"),
      x = dplyr::last(route_data$longitude),
      y = dplyr::last(route_data$latitude),
      family = "fontawesome-webfont",
      size = 6
    ) +
    viridis::scale_color_viridis(option = "plasma", direction = -1) +
    ggplot2::labs(x = "Longitude", y = "Latitude", color = "Prop. Trip\nCompleted\n") +
    ggplot2::theme(legend.position = ifelse(diff(range(route_data$longitude)) > diff(range(route_data$latitude)),
                                   "bottom", "right"))

  if (!missing(layers)) {
    map <- map + layers
  }

  return(map)
}
