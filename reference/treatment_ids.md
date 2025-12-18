# Identify treatment ids based on a user-defined window

If in treatment falls within the window, it is considered a single
treatment event and a new treatment id is created.

## Usage

``` r
treatment_ids(id, time, treatment, window, time_units = "days")
```

## Arguments

- id:

  A column name containing the unique identifier of the observational
  unit in the panel (the i dimension)

- time:

  A column name containing the time variable in the panel (i.e. the t
  dimension)

- treatment:

  A column name containing the name of the dummy variable indicating
  whether unit i is treated at time t

- window:

  A number indicating the number of periods within which two or more
  treatment events are merged. This should be in the same units as
  `time`

- time_units:

  A character vector specifying the units in which `time` is measured.
  Default is "days", other options are "weeks", "months", or "years"

## Value

A character vector specifying the new treatment ids

## Examples

``` r
# Load packages
library(tidyr)
library(dplyr)
library(EventHorizon)

# Load a panel
panel <- simulate_panel(n_units = 10, n_periods = 10)
head(panel)
#> # A tibble: 6 × 3
#>   id    time       treatment
#>   <chr> <date>         <int>
#> 1 A1    2025-01-01         0
#> 2 A1    2025-01-02         0
#> 3 A1    2025-01-03         0
#> 4 A1    2025-01-04         1
#> 5 A1    2025-01-05         0
#> 6 A1    2025-01-06         0

event_horizon_panel <- panel |>
mutate(treatment_id = treatment_ids(id = id,
                                    time = time,
                                    treatment = treatment,
                                    window = 3))

head(event_horizon_panel)
#> # A tibble: 6 × 4
#>   id    time       treatment treatment_id  
#>   <chr> <date>         <int> <chr>         
#> 1 A1    2025-01-01         0 NA            
#> 2 A1    2025-01-02         0 NA            
#> 3 A1    2025-01-03         0 NA            
#> 4 A1    2025-01-04         1 treatment_A1_1
#> 5 A1    2025-01-05         0 treatment_A1_1
#> 6 A1    2025-01-06         0 treatment_A1_1
```
