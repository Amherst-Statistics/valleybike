#' Identify trips
#'
#' Identify the route IDs of specific trips using information about the date they were undertaken,
#' the start station, and/or the end station. One can provide only one or all of the arguments.
#'
#' @title identify_trips
#'
#' @param date The date that the trip was undertaken (as a YYYY-MM-DD string).
#' @param start_station The start station of the trip (as a string).
#' @param end_station The end station of the trip (as a string).
#'
#' @return A vector of route IDs that match the specified search criteria.
#'
#' @examples
#' \dontrun{
#' identify_trips(date = "2019-07-12", start_station = "Amherst Town Hall")
#' }
#'
#' @export
identify_trips <- function(date = NULL, start_station = NULL, end_station = NULL) {

  if (is.null(date) && is.null(start_station) && is.null(end_station)) {
    stop("At least one argument must be provided.")
  }

  utils::data("trips", package = "valleybikeData", envir = environment())

  route_ids <- trips %>%
    {if (!is.null(date)) dplyr::filter(., format(start_time, format = "%Y-%m-%d") == date) else .} %>%
    {if (!is.null(start_station)) dplyr::filter(., start_station == !!start_station) else .} %>%
    {if (!is.null(end_station)) dplyr::filter(., end_station == !!end_station) else .} %>%
    dplyr::pull(route_id)

  return(route_ids)
}
