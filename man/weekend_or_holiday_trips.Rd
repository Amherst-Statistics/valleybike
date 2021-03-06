% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weekend-or-holiday.R
\name{weekend_or_holiday_trips}
\alias{weekend_or_holiday_trips}
\title{weekend_or_holiday_trips}
\usage{
weekend_or_holiday_trips(user_id)
}
\arguments{
\item{user_id}{A user's ID (as a string).}
}
\value{
TRUE if the trip was undertaken on a weekend or federal holiday, FALSE otherwise.
}
\description{
Number of weekend or holiday trips
}
\details{
Determines the number of trips by a specific user that were undertaken on a weekend
or on a federal holiday.
}
\examples{
\dontrun{
weekend_or_holiday_trips("1cc1e858-857a-4f80-96e8-974f0e920620")
}

}
