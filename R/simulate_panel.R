#' Build a simulated panel
#'
#' @param n_units An integer specifying the number of observational units
#' @param n_periods An integer specifying the number of periods
#'
#' @returns a data.frame containing a panel with three columns for id, time, and treatment
#' @export
#'
#' @examples
#'
#' #' # Load a panel
#' panel <- simulate_panel(n_units = 10, n_periods = 10)
#' head(panel)
#
simulate_panel <- function(n_units = 30, n_periods = 50){


  set.seed(1)
  tidyr::expand_grid(id = outer(LETTERS,
                                1:ceiling(n_units / length(LETTERS)),
                                paste0)[1:n_units],
                     time = 1:n_periods) |>
    dplyr::mutate(treatment = rbinom(n = id, size = 1, prob = 0.1))
}
