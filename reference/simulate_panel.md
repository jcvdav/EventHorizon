# Build a simulated panel

Build a simulated panel

## Usage

``` r
simulate_panel(n_units = 30, n_periods = 30)
```

## Arguments

- n_units:

  An integer specifying the number of observational units

- n_periods:

  An integer specifying the number of periods (assumed to be days, max
  30)

## Value

a data.frame containing a panel with three columns for id, time, and
treatment

## Examples

``` r
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
```
