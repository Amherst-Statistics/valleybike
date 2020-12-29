#' Weekend or holiday
#'
#' Determines whether a trip was undertaken on a weekend or on a federal holiday.
#'
#' @title weekend_or_holiday
#'
#' @param route_id A trip's route ID (as a string).
#'
#' @return TRUE if the trip was undertaken on a weekend or federal holiday, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' weekend_or_holiday("route_04_2019@01253ad9-cfb8-44a5-9218-bff8109e4880")
#' }
#'
#' @export
weekend_or_holiday <- function(route_id) {

  start_date <- valleybikeData::trips %>%
    dplyr::filter(route_id == !!route_id) %>%
    dplyr::pull(start_time) %>%
    format(format = "%Y-%m-%d")

  weekend_days <- c("Saturday", "Sunday")
  is_weekend <- lubridate::wday(start_date, label = TRUE, abbr = FALSE) %in% weekend_days

  holidays <- c(
    "2018-05-28", "2019-05-27", "2020-05-25", "2021-05-31", "2022-05-30", # Memorial Day
    "2018-07-04", "2019-07-04", "2020-07-04", "2021-07-04", "2022-07-04", # Independence Day
    "2018-09-03", "2019-09-02", "2020-09-07", "2021-09-06", "2022-09-05", # Labor Day
    "2018-10-08", "2019-10-14", "2020-10-12", "2021-10-11", "2022-10-10", # Columbus Day
    "2018-11-11", "2019-11-11", "2020-11-11", "2021-11-11", "2022-11-11", # Veterans Day
    "2018-11-22", "2019-11-28", "2020-11-26", "2021-11-25", "2022-11-24"  # Thanksgiving Day
  )
  is_holiday <- start_date %in% holidays

  return(is_weekend || is_holiday)
}

#' Number of weekend or holiday trips
#'
#' Determines the number of trips by a specific user that were undertaken on a weekend
#' or on a federal holiday.
#'
#' @title weekend_or_holiday_trips
#'
#' @param user_id A user's ID (as a string).
#'
#' @return TRUE if the trip was undertaken on a weekend or federal holiday, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' weekend_or_holiday_trips("1cc1e858-857a-4f80-96e8-974f0e920620")
#' }
#'
#' @export
weekend_or_holiday_trips <- function(user_id) {

  num_trips <- valleybikeData::trips %>%
    dplyr::filter(user_id == !!user_id) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(weekend_or_holiday = weekend_or_holiday(route_id)) %>%
    dplyr::ungroup() %>%
    dplyr::summarize(num_trips = sum(weekend_or_holiday)) %>%
    dplyr::pull(num_trips)

  return(num_trips)
}
