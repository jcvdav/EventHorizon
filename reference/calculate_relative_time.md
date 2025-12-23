# Title

Title

## Usage

``` r
calculate_relative_time(id, treatment_id, window)
```

## Arguments

- id:

  A column name containing the unique identifier of the observational
  unit in the panel (the i dimension)

- treatment_id:

  A character specifying the treatment id, as produced by
  [`treatment_ids()`](https://jcvdav.github.io/EventHorizon/reference/treatment_ids.md)

- window:

  A number indicating the number of periods within which two or more
  treatment events are merged

## Value

A numeric vector containing the relative time of treatment in number of
periods before or after the start and end of treatment ids identified by
[`treatment_ids()`](https://jcvdav.github.io/EventHorizon/reference/treatment_ids.md)

## See also

[`treatment_ids()`](https://jcvdav.github.io/EventHorizon/reference/treatment_ids.md)

## Examples

``` r
# Load packages
library(tidyr)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
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
                                    window = 3),
       relative_time = calculate_relative_time(id = id,
                                               treatment_id = treatment_id,
                                               window = 3))

head(event_horizon_panel)
#> # A tibble: 6 × 5
#>   id    time       treatment treatment_id   relative_time
#>   <chr> <date>         <int> <chr>                  <dbl>
#> 1 A1    2025-01-01         0 NA                        -3
#> 2 A1    2025-01-02         0 NA                        -2
#> 3 A1    2025-01-03         0 NA                        -1
#> 4 A1    2025-01-04         1 treatment_A1_1             0
#> 5 A1    2025-01-05         0 treatment_A1_1             0
#> 6 A1    2025-01-06         0 treatment_A1_1             0
```
