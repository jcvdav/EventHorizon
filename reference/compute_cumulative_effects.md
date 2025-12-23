# Compute cumulative event-study effects with delta-method standard errors.

Compute cumulative event-study effects with delta-method standard
errors.

## Usage

``` r
compute_cumulative_effects(
  model,
  prefix = "relative_time::",
  start = 0,
  level = 0.95,
  which = 1
)
```

## Arguments

- model:

  A `fixest` object or a `coeftest` object (as returned by `conleyreg`).

- prefix:

  Character prefix identifying the event-study coefficients. The default
  matches `i(relative_time, ...)` produced names such as
  `"relative_time::0"`.

- start:

  Integer relative time at which cumulative sums begin. Values less than
  `start` are excluded before summing. If `i(relative_time, -1)` was
  used, then we suggest you use -1 as an input here.

- level:

  Confidence level (default 0.95).

- which:

  If `model` is `fixest_multi`, selects the component to use.

## Value

A tibble with columns `relative_time`, `term`, `cum_effect`,
`std_error`, `conf.low`, and `conf.high`. If coefficients contain
categorical interactions, a `category` column is also included. The
`term` column contains the original coefficient names.

## Details

The function forms cumulative effects by left-multiplying the vector of
event-time estimates with a lower-triangular matrix of ones. Let `β` be
the vector of selected event-study coefficients and `Σ` their covariance
matrix. The cumulative effect at horizon `h` is `c_h = L_h β`, where
`L_h` sums coefficients up through `h`. The associated variance is
`Var(c_h) = L_h Σ L_h'`, which is implemented via matrix multiplication.
The reported standard errors are the square roots of these variances
(delta method), and confidence intervals use the normal critical value.
