
# valleybike <img src="man/figures/logo.png" title="logo created with hexSticker" width="160px" align="right"/>

<!-- badges: start -->

[![R build
status](https://github.com/Amherst-Statistics/valleybike/workflows/R-CMD-check/badge.svg)](https://github.com/Amherst-Statistics/valleybike/actions)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

[ValleyBike.org](https://www.valleybike.org/) utility functions. To be
used in conjunction with the `valleybikeData` package (more details
[here](https://github.com/Amherst-Statistics/valleybikeData)).

## Installation

Install the development version from GitHub:

``` r
devtools::install_github("Amherst-Statistics/valleybike")
library(valleybike)
```

## Functions

The package includes a variety of utility functions for working with the
ValleyBike data:

  - `get_distance` (get the distance covered by a trip)
  - `get_duration` (get the duration of a trip)
  - `get_summary_report` (get a .pdf summary report for a specific town)
  - `get_trajectory_data` (get the point-by-point trajectory data for a
    trip)
  - `identify_trips` (identify the route ID of a trip about which
    details are known)
  - `plot_trip` (plot a trip on the map)
  - `weekend_or_holiday` (check whether a trip took place during a
    weekend or a holiday)
  - `weekend_or_holiday_trips` (get the number of weekend or holiday
    trips undertaken by a user)

For more details on what these functions do and how to use them, please
see the [package
manual](https://github.com/Amherst-Statistics/valleybike/blob/main/valleybike_0.0.1.pdf).
