#' Compute cumulative event-study effects with delta-method standard errors.
#'
#' @param model A `fixest` object or a `coeftest` object (as returned by `conleyreg`).
#' @param prefix Character prefix identifying the event-study coefficients. The
#'   default matches `i(relative_time, ...)` produced names such as
#'   `"relative_time::0"`.
#' @param start Integer relative time at which cumulative sums begin. Values
#'   less than `start` are excluded before summing. A reference row is added
#'   at `start` with cumulative effect = 0 (the omitted reference category).
#'   If `i(relative_time, -1)` was used, then we suggest you use -1 as an input here.
#' @param level Confidence level (default 0.95).
#' @param which If `model` is `fixest_multi`, selects the component to use.
#'
#' @return A tibble with columns `relative_time`, `term`, `cum_effect`,
#'   `std_error`, `conf.low`, and `conf.high`. If coefficients contain
#'   categorical interactions, a `category` column is also included. The
#'   `term` column contains the original coefficient names.
#'
#' @importFrom fixest coeftable
#' @importFrom stringr str_starts str_remove str_remove_all str_detect str_extract
#' @importFrom readr parse_number
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
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
  term_names <- names(coefs)  # Store original coefficient names

  # Check for NaN or NA values in vcov matrix and notify user
  has_nan <- any(is.nan(vcov_mat))
  has_na <- any(is.na(vcov_mat))
  if (has_nan || has_na) {
    warning("The input model contains NA or NaN values in standard errors. ",
            "These have been replaced with zeros to allow delta method calculation to proceed. ",
            "Coefficients with missing standard errors will not contribute to cumulative variance calculations.")
  }

  # Handle NaN/NA values in vcov matrix by replacing with 0
  # This allows delta method calculation to proceed even when some SEs are NaN/NA
  vcov_mat[is.nan(vcov_mat) | is.na(vcov_mat)] <- 0

  # Parse event-time indices and detect categorical interactions.
  # Pattern: prefix:event_time or prefix:event_time:category_var::category_level
  coef_names_clean <- names(coefs) |>
    stringr::str_remove(prefix) |>
    stringr::str_remove_all("[()]")
  
  # Check if there are interaction terms (contain "::" after the event time)
  has_interactions <- any(stringr::str_detect(coef_names_clean, "::"))
  
  if (has_interactions) {
    # Extract event time (first number) and category (after last "::")
    event_time <- coef_names_clean |>
      stringr::str_extract("^-?\\d+") |>
      readr::parse_number()
    
    # Extract category level (everything after the last "::")
    category <- coef_names_clean |>
      stringr::str_extract("::([^:]+)$") |>
      stringr::str_remove("^::")
    
    # If category is NA, it means no interaction pattern matched, treat as single group
    if (any(is.na(category))) {
      category <- rep(NA_character_, length(coef_names_clean))
      has_interactions <- FALSE
    }
  } else {
    # No interactions: just extract event time
    event_time <- coef_names_clean |>
      readr::parse_number()
    category <- rep(NA_character_, length(coef_names_clean))
  }

  # If interactions detected, compute cumulative effects separately for each category
  if (has_interactions && !all(is.na(category))) {
    # Group by category and compute cumulative effects for each group
    unique_categories <- unique(category[!is.na(category)])
    results_list <- list()
    
    for (cat in unique_categories) {
      # Get indices for this category
      cat_indices <- which(category == cat)
      
      # Extract coefficients and vcov for this category
      coefs_cat <- coefs[cat_indices]
      vcov_cat <- vcov_mat[cat_indices, cat_indices, drop = FALSE]
      event_time_cat <- event_time[cat_indices]
      term_names_cat <- term_names[cat_indices]
      
      # Order by event time within this category
      ord_cat <- order(event_time_cat)
      coefs_cat <- coefs_cat[ord_cat]
      vcov_cat <- vcov_cat[ord_cat, ord_cat, drop = FALSE]
      event_time_cat <- event_time_cat[ord_cat]
      term_names_cat <- term_names_cat[ord_cat]
      
      # Drop event times earlier than start
      keep_cat <- event_time_cat >= start
      if (!any(keep_cat)) {
        next  # Skip this category if no valid times
      }
      
      coefs_cat <- coefs_cat[keep_cat]
      vcov_cat <- vcov_cat[keep_cat, keep_cat, drop = FALSE]
      event_time_cat <- event_time_cat[keep_cat]
      term_names_cat <- term_names_cat[keep_cat]
      
      # Compute cumulative effects for this category
      n_cat <- length(coefs_cat)
      L_cat <- matrix(0, nrow = n_cat, ncol = n_cat)
      L_cat[lower.tri(L_cat, diag = TRUE)] <- 1
      
      cum_effect_cat <- as.numeric(L_cat %*% coefs_cat)
      cum_var_cat <- diag(L_cat %*% vcov_cat %*% t(L_cat))
      cum_var_cat[cum_var_cat < 0 | is.nan(cum_var_cat)] <- 0
      std_error_cat <- sqrt(cum_var_cat)
      
      crit <- stats::qnorm(1 - (1 - level) / 2)
      
      # Create reference row at start (omitted reference category)
      # Construct term name for reference period
      # Extract the category part (everything after the event time in the first term)
      first_term <- term_names_cat[1]
      # Pattern: prefix:event_time:category_part or prefix:event_time
      # Remove prefix and event time, keep the rest
      category_part <- stringr::str_remove(first_term, paste0("^", prefix, "-?\\d+"))
      if (category_part == "" || is.na(category_part)) {
        ref_term <- paste0(prefix, start)
      } else {
        # Remove leading ":" if present, then add start and category part
        category_part <- stringr::str_remove(category_part, "^:")
        ref_term <- paste0(prefix, start, ":", category_part)
      }
      
      # Combine reference row with computed cumulative effects
      results_list[[cat]] <- dplyr::bind_rows(
        tibble::tibble(
          relative_time = start,
          term = ref_term,
          category = cat,
          cum_effect = 0,
          std_error = 0,
          conf.low = 0,
          conf.high = 0
        ),
        tibble::tibble(
          relative_time = event_time_cat,
          term = term_names_cat,
          category = cat,
          cum_effect = cum_effect_cat,
          std_error = std_error_cat,
          conf.low = cum_effect_cat - crit * std_error_cat,
          conf.high = cum_effect_cat + crit * std_error_cat
        )
      )
    }
    
    # Combine results from all categories
    return(dplyr::bind_rows(results_list))
  } else {
    # No interactions: proceed with original logic
    # Order coefficients chronologically by event time.
    ord <- order(event_time)
    coefs <- coefs[ord]
    vcov_mat <- vcov_mat[ord, ord, drop = FALSE]
    event_time <- event_time[ord]
    term_names <- term_names[ord]  # Keep names aligned with ordering

    # Drop event times earlier than the specified starting point.
    keep <- event_time >= start
    if (!any(keep)) {
      stop("No event times >= start (", start, ").")
    }

    coefs <- coefs[keep]
    vcov_mat <- vcov_mat[keep, keep, drop = FALSE]
    event_time <- event_time[keep]
    term_names <- term_names[keep]  # Keep names aligned with filtering

    # Build lower-triangular summation matrix for cumulative effects.
    n <- length(coefs)
    L <- matrix(0, nrow = n, ncol = n)
    L[lower.tri(L, diag = TRUE)] <- 1

    # Compute cumulative effects and associated standard errors.
    cum_effect <- as.numeric(L %*% coefs)
    cum_var <- diag(L %*% vcov_mat %*% t(L))
    # Handle cases where variance might be negative or NaN due to numerical issues
    # Replace with 0 to allow sqrt calculation to proceed
    cum_var[cum_var < 0 | is.nan(cum_var)] <- 0
    std_error <- sqrt(cum_var)

    # Form confidence intervals using the normal critical value.
    crit <- stats::qnorm(1 - (1 - level) / 2)

    # Create reference row at start (omitted reference category)
    ref_term <- paste0(prefix, start)
    
    # Combine reference row with computed cumulative effects
    dplyr::bind_rows(
      tibble::tibble(
        relative_time = start,
        term = ref_term,
        cum_effect = 0,
        std_error = 0,
        conf.low = 0,
        conf.high = 0
      ),
      tibble::tibble(
        relative_time = event_time,
        term = term_names,
        cum_effect = cum_effect,
        std_error = std_error,
        conf.low = cum_effect - crit * std_error,
        conf.high = cum_effect + crit * std_error
      )
    )
  }
}
