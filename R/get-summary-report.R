# =================================================================================================
# ADD START AND END CITIES TO TRIPS DATASET
# =================================================================================================

.stations_clean <- valleybikeData::stations %>%
  dplyr::select(name, city)

.trips <- valleybikeData::trips %>%
  dplyr::left_join(.stations_clean, by = c("start_station" = "name")) %>%
  dplyr::rename(start_city = city) %>%
  dplyr::left_join(.stations_clean, by = c("end_station" = "name")) %>%
  dplyr::rename(end_city = city)

# =================================================================================================
# WHEN STATIONS WENT ONLINE AND OFFLINE
# =================================================================================================

.summarize_online_offline <- function(city, year) {

  city_stations <- valleybikeData::stations %>%
    dplyr::filter(city == !!city) %>%
    dplyr::pull(name)

  summary <- .trips %>%
    dplyr::filter(
      start_city == city | end_city == city,
      format(start_time, "%Y") == year
    ) %>%
    tidyr::pivot_longer(cols = c(start_station, end_station), names_to = "type", values_to = "station") %>%
    dplyr::filter(station %in% city_stations) %>%
    dplyr::mutate(type = ifelse(type == "start_station", "departure", "arrival")) %>%
    dplyr::group_by(station) %>%
    dplyr::summarize(
      first_arrival = min(end_time[type == "arrival"]),
      first_departure = min(start_time[type == "departure"]),
      last_arrival = max(end_time[type == "arrival"]),
      last_departure = max(start_time[type == "departure"]),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(lubridate::is.POSIXct, ~format(., "%Y-%m-%d")))

  summary <- knitr::kable(
    summary,
    format = "latex",
    booktabs = TRUE,
    col.names = c("Station", "First Arrival", "First Departure", "Last Arrival", "Last Departure"),
    caption = paste("First and last trip at each station in", city, "for the year", year),
    linesep = ""
  ) %>%
    kableExtra::kable_styling(font_size = 9, latex_options = c("striped", "HOLD_position")) %>%
    kableExtra::add_footnote("The symbol ** indicates a missing value", notation = "none")

  return(summary)
}

# =================================================================================================
# TRIP SUMMARIES BY STATION
# =================================================================================================

.summarize_trips <- function(city, year, min_duration = 60, max_duration = 14400) {

  summary <- .trips %>%
    dplyr::filter(
      start_city == city,
      format(start_time, "%Y") == year
    ) %>%
    dplyr::mutate(
      is_short = (duration <= min_duration),
      is_long = (duration >= max_duration),
      is_return = (start_station == end_station),
      is_to_nowhere = is.na(end_station)
    ) %>%
    dplyr::group_by(start_station) %>%
    dplyr::summarize(
      n = dplyr::n(),
      prop_short = is_short %>%
        mean(na.rm = TRUE) %>%
        {. * 100} %>%
        round(2),
      prop_long = is_long %>%
        mean(na.rm = TRUE) %>%
        {. * 100} %>%
        round(2),
      prop_return = is_return %>%
        mean(na.rm = TRUE) %>%
        {. * 100} %>%
        round(2),
      prop_to_nowhere = is_to_nowhere %>%
        mean(na.rm = TRUE) %>%
        {. * 100} %>%
        round(2),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(n))

  summary <- knitr::kable(
    summary,
    format = "latex",
    booktabs = TRUE,
    col.names = c("Start Station", "Total Trips", "% Short Trips", "% Long Trips",
                  "% Return Trips", "% Trips to Nowhere"),
    caption = paste("Summary of Trips in", city, "for", year),
    linesep = ""
  ) %>%
    kableExtra::kable_styling(font_size = 8, latex_options = c("striped", "HOLD_position")) %>%
    kableExtra::add_footnote("The symbol ** indicates a missing value", notation = "none") %>%
    kableExtra::add_footnote(paste("Trips shorter than", round(min_duration / 60, 2),
                                   "minute(s) are considered short, and trips longer than",
                                   round(max_duration / 3600, 2), "hour(s) are considered long"),
                             notation = "none")

  return(summary)
}

# =================================================================================================
# SUMMARY OF END STATIONS
# =================================================================================================

.summarize_end_stations <- function(city, year, min_duration = 60, max_duration = 14400) {

  city_stations <- valleybikeData::stations %>%
    dplyr::filter(city == !!city) %>%
    dplyr::pull(name)

  summarize_one_station <- function(start_station) {

    summary <- .trips %>%
      dplyr::filter(
        start_station == !!start_station,
        format(start_time, "%Y") == year,
        dplyr::between(duration, min_duration, max_duration)
      ) %>%
      dplyr::group_by(end_station) %>%
      dplyr::summarize(
        n = dplyr::n(),
        mean_duration = duration %>%
          {. / 60} %>%
          mean(na.rm = TRUE) %>%
          round(2),
        median_duration = duration %>%
          {. / 60} %>%
          stats::median(na.rm = TRUE) %>%
          round(2),
        sd_duration = duration %>%
          {. / 60} %>%
          stats::sd(na.rm = TRUE) %>%
          round(2),
        .groups = "drop"
      ) %>%
      dplyr::slice_max(n, n = 10)

    summary <- knitr::kable(
      summary,
      format = "latex",
      booktabs = TRUE,
      col.names = c("End Station", "Total Trips", "Mean Duration",
                    "Median Duration", "St. Dev. of Duration"),
      caption = paste("Top 10 End Stations from", start_station, "in", city, "for", year),
      linesep = ""
    ) %>%
      kableExtra::kable_styling(font_size = 9, latex_options = c("striped", "HOLD_position")) %>%
      kableExtra::add_footnote("NOTE: All durations are in minutes", notation = "none") %>%
      kableExtra::add_footnote("The symbol ** indicates a missing value", notation = "none") %>%
      kableExtra::add_footnote(
        paste("Short trips are trips deemed too short to be valid. Here, the cutoff is",
              round(min_duration / 60, 2), "minute(s)."),
        notation = "none"
      )

    return(summary)
  }

  summaries <- lapply(city_stations, summarize_one_station)

  return(summaries)
}

# =================================================================================================
# SUMMARY OF TOP 10 PAIRS OF STATIONS
# =================================================================================================

.summarize_station_pairs <- function(city, year, min_duration = 60, max_duration = 14400) {

  summary <- .trips %>%
    dplyr::filter(
      start_city == city,
      end_city == city,
      format(start_time, "%Y") == year,
      dplyr::between(duration, min_duration, max_duration)
    ) %>%
    dplyr::group_by(start_station, end_station) %>%
    dplyr::summarize(
      n = dplyr::n(),
      mean_duration = duration %>%
        {. / 60} %>%
        mean(na.rm = TRUE) %>%
        round(2),
      median_duration = duration %>%
        {. / 60} %>%
        stats::median(na.rm = TRUE) %>%
        round(2),
      sd_duration = duration %>%
        {. / 60} %>%
        stats::sd(na.rm = TRUE) %>%
        round(2),
      .groups = "drop"
    ) %>%
    dplyr::slice_max(n, n = 10)

  summary <- knitr::kable(
    summary,
    format = "latex",
    booktabs = TRUE,
    col.names = c("Start Station", "End Station", "Total Trips", "Mean Duration",
                  "Median Duration", "St. Dev. Duration"),
    caption = paste("Top 10 Pairs of Stations in", city, "for", year),
    linesep = ""
  ) %>%
    kableExtra::kable_styling(font_size = 8, latex_options = c("striped", "HOLD_position")) %>%
    kableExtra::add_footnote("NOTE: All durations are in minutes", notation = "none") %>%
    kableExtra::add_footnote("The symbol ** indicates a missing value", notation = "none")  %>%
    kableExtra::add_footnote(paste("Trips shorter than", round(min_duration / 60, 2),
                                   "minute(s), and trips longer than", round(max_duration / 3600, 2),
                                   "have not been counted"),
                             notation = "none")

  return(summary)
}

#' Get summary report
#'
#' Generate a .pdf summary report for a city in which ValleyBike is active.
#'
#' @title get_summary_report
#'
#' @param city The city for which to generate the summary report (as a string). Can be one of:
#'             "Amherst", "Springfield", "Easthampton", "Northampton", "Holyoke", or "South Hadley".
#' @param filename The file name to use for the generated .pdf report.
#' @param path The path where to save the generated report.
#' @param overwrite Whether to overwrite any existing file with the same name at the specified
#'                  path. TRUE will overwrite existing files, FALSE will leave existing files
#'                  alone and issue an error. Defaults to FALSE.
#'
#' @examples
#' \dontrun{
#' get_summary_report(city = "Amherst", filename = "amherst-report.pdf")
#' }
#'
#' @export
get_summary_report <- function(city, filename, path = ".", overwrite = FALSE) {

  cities <- c("Amherst", "Springfield", "Easthampton", "Northampton", "Holyoke", "South Hadley")
  years <- valleybikeData::trips$start_time %>%
    format(format = "%Y") %>%
    unique()

  if (!(city %in% cities)) {
    stop("Invalid city name. Must be one of: ", paste(cities, collapse = ", "), ".")
  }

  if (missing(filename)) {
    city_no_whitespace <- gsub(" ", "-", tolower(city))
    filename <- paste0(city_no_whitespace, "-station-report.Rmd")
  }

  destination_file <- file.path(path, filename)

  if (file.exists(destination_file) & !overwrite) {
    stop("File \"", destination_file, "\" already exists. To overwrite it, set overwrite = TRUE.\n")
  }

  header_file <- system.file("extdata", "report-header.txt", package = "valleybike")

  cat("---\n", file = destination_file)
  cat(paste("title: Station Summary Report for", city, "\n"), file = destination_file, append = TRUE)
  file.append(destination_file, header_file)

  output_summaries <- function(year) {

    cat("\\newpage", file = destination_file, append = TRUE)

    cat("\n\n# Summary for", year, "\n\n", file = destination_file, append = TRUE)

    # ONLINE AND OFFLINE ==========================================================================

    cat("\n\n## Online and Offline Dates by Station\n\n", file = destination_file, append = TRUE)

    online_offline_summary <- .summarize_online_offline(city, year)
    cat(online_offline_summary, file = destination_file, append = TRUE)

    cat("\\newpage", file = destination_file, append = TRUE)

    # TRIP SUMMARIES ==============================================================================

    cat("\n\n## Trip Summaries by Station\n\n", file = destination_file, append = TRUE)

    trips_summary <- .summarize_trips(city, year)
    cat(trips_summary, file = destination_file, append = TRUE)

    cat("\\newpage", file = destination_file, append = TRUE)

    # TOP DESTINATIONS ============================================================================

    cat("\n\n## Top Destinations by Start Station\n\n", file = destination_file, append = TRUE)

    end_stations_summaries <- .summarize_end_stations(city, year)
    lapply(end_stations_summaries, function(summary) {
      cat(summary, file = destination_file, append = TRUE)
    })

    cat("\\newpage", file = destination_file, append = TRUE)

    # TOP STATION PAIRS ===========================================================================

    cat("\n\n## Top 10 Station Pairs\n\n", file = destination_file, append = TRUE)

    station_pairs_summary <- .summarize_station_pairs(city, year)
    cat(station_pairs_summary, file = destination_file, append = TRUE)
  }

  lapply(years, output_summaries)

  rmarkdown::render(destination_file, output_format = "pdf_document")
}
