#' Compute cumulative event-study effects with delta-method standard errors.
#'
#' @param model A `fixest` object or a `coeftest` object (as returned by `conleyreg`).
#' @param prefix Character prefix identifying the event-study coefficients. The
#'   default matches `i(relative_time, ...)` produced names such as
#'   `"relative_time::0"`.
#' @param start Integer relative time at which cumulative sums begin. Values
#'   less than `start` are excluded before summing. If `i(relative_time, -1)`
#'   was used, then we suggest you use -1 as an input here.
#' @param level Confidence level (default 0.95).
#' @param which If `model` is `fixest_multi`, selects the component to use.
#'
#' @return A tibble with columns `relative_time`, `cum_effect`, `std_error`,
#'   `conf_low`, and `conf_high`.
#'
#' @details The function forms cumulative effects by left-multiplying the vector
#'   of event-time estimates with a lower-triangular matrix of ones. Let `β` be
#'   the vector of selected event-study coefficients and `Σ` their covariance
#'   matrix. The cumulative effect at horizon `h` is `c_h = L_h β`, where `L_h`
#'   sums coefficients up through `h`. The associated variance is
#'   `Var(c_h) = L_h Σ L_h'`, which is implemented via matrix multiplication. The
#'   reported standard errors are the square roots of these variances (delta
#'   method), and confidence intervals use the normal critical value.
compute_cumulative_effects <- function(model,
                                       prefix = "relative_time::",
                                       start = 0,
                                       level = 0.95,
                                       which = 1) {
  if (inherits(model, "fixest_multi")) {
    message("You gave me a fixest_multi, but I can only work with fixest objects.
            Consider using [[]] or map_dfr instead.")
  }

  # Check if model is from fixest or coeftest (conleyreg)
  is_fixest <- inherits(model, "fixest")
  is_coeftest <- inherits(model, "coeftest")
  
  if (!is_fixest && !is_coeftest) {
    stop("Model must be a 'fixest' object or a 'coeftest' object (as returned by conleyreg).")
  }

  # Retrieve coefficient estimates and their covariance matrix.
  # For fixest, use coeftable; for coeftest, extract from matrix columns
  if (is_fixest) {
    ct <- fixest::coeftable(model)
    coefs <- ct[, "Estimate"]
    vcov_mat <- stats::vcov(model)
  } else {
    # coeftest objects are matrices: first column = coefficients, second = std errors
    coefs <- model[, 1]
    names(coefs) <- rownames(model)
    # Try to get vcov, otherwise construct from standard errors
    vcov_mat <- tryCatch(stats::vcov(model), error = function(e) NULL)
    if (is.null(vcov_mat)) {
      # Construct diagonal vcov matrix from standard errors
      std_errors <- model[, 2]
      vcov_mat <- diag(std_errors^2)
      rownames(vcov_mat) <- colnames(vcov_mat) <- rownames(model)
    }
  }

  # Identify event-study coefficients matching the requested prefix.
  relevant <- stringr::str_starts(names(coefs), prefix)

  if (!any(relevant)) {
    stop("No coefficients found with prefix '", prefix, "'.")
  }

  # Keep only the relevant coefficients and corresponding covariance rows/cols.
  coefs <- coefs[relevant]
  vcov_mat <- vcov_mat[relevant, relevant, drop = FALSE]

  # Parse event-time indices from coefficient names.
  event_time <- names(coefs) |>
    stringr::str_remove(prefix) |>
    stringr::str_remove_all("[()]") |>
    readr::parse_number()

  # Order coefficients chronologically by event time.
  ord <- order(event_time)
  coefs <- coefs[ord]
  vcov_mat <- vcov_mat[ord, ord, drop = FALSE]
  event_time <- event_time[ord]

  # Drop event times earlier than the specified starting point.
  keep <- event_time >= start
  if (!any(keep)) {
    stop("No event times >= start (", start, ").")
  }

  coefs <- coefs[keep]
  vcov_mat <- vcov_mat[keep, keep, drop = FALSE]
  event_time <- event_time[keep]

  # Build lower-triangular summation matrix for cumulative effects.
  n <- length(coefs)
  L <- matrix(0, nrow = n, ncol = n)
  L[lower.tri(L, diag = TRUE)] <- 1

  # Compute cumulative effects and associated standard errors.
  cum_effect <- as.numeric(L %*% coefs)
  cum_var <- diag(L %*% vcov_mat %*% t(L))
  std_error <- sqrt(cum_var)

  # Form confidence intervals using the normal critical value.
  crit <- stats::qnorm(1 - (1 - level) / 2)

  tibble::tibble(
    relative_time = event_time,
    cum_effect = cum_effect,
    std_error = std_error,
    conf_low = cum_effect - crit * std_error,
    conf_high = cum_effect + crit * std_error
  )
}
