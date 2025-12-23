#' Assign id to leads and lags.
#'
#' @description
#' Propagates the treatment id from `treatment_ids` across control periods
#'
#'
#' @param id A column name containing the unique identifier of the observational unit in the panel (the i dimension)
#' @param time A column name containing the time variable in the panel (i.e. the t dimension)
#' @param treatment_id A character specifying the treatment id, as produced by `treatment_ids()`
#' @param window A number indicating the number of periods within which two or more treatment events are merged. This should be in the same units as `time`
#'
#' @returns A character vector specifiying the treatment id for treated and control periods of a given unit
#' @export
#'
#' @examples
#' # Load packages
#' library(tidyr)
#' library(dplyr)
#' library(EventHorizon)
#'
#' # Load a panel
#' panel <- simulate_panel(n_units = 10, n_periods = 10)
#' head(panel)
#'
#' event_horizon_panel <- panel |>
#' mutate(treatment_id = treatment_ids(id = id,
#'                                     time = time,
#'                                     treatment = treatment,
#'                                     window = 3),
#'        relative_time = calculate_relative_time(id = id,
#'                                                treatment_id = treatment_id,
#'                                                window = 3),
#'        treatment_id = propagate_treatment_id(id = id,
#'                                              time = time,
#'                                              treatment_id = treatment_id,
#'                                              window = 3)) |>
#' drop_na(treatment_id)
#'
#' head(event_horizon_panel)
#' visualize_panel(panel)
#' visualize_panel(event_horizon_panel)
#' visualize_panel(event_horizon_panel, prepped = TRUE)
#
propagate_treatment_id <- function(time, id, treatment_id, window) {
  # Ensure input vectors are of the same length
  if (length(time) != length(treatment_id) || length(time) != length(id)) {
    stop("time, id, and treatment_id must have the same length.")
  }

  # Initialize a vector to store the propagated treatment IDs
  propagated_treatment_id <- treatment_id

  # Iterate over each unique `id`
  unique_ids <- unique(id)

  for (current_id in unique_ids) {
    # Get indices for the current `id`
    id_indices <- which(id == current_id)

    # Extract relevant data for the current `id`
    current_treatment_id <- treatment_id[id_indices]

    # Iterate through each index in the current `id`
    for (i in seq_along(current_treatment_id)) {
      if (!is.na(current_treatment_id[i])) {  # If the current day has an treatment ID assigned
        # Propagate to the previous days (negative leads, for the same id)
        if (i > 1) {  # Ensure there are previous indices to propagate to
          for (j in seq(i - 1, max(1, i - window), by = -1)) {
            if (is.na(propagated_treatment_id[id_indices[j]])) {
              propagated_treatment_id[id_indices[j]] <- current_treatment_id[i]
            } else {
              break  # Stop if propagation encounters an already-filled treatment_id
            }
          }
        }

        # Propagate to the next days (positive lags, for the same id)
        if (i < length(current_treatment_id)) {  # Ensure there are next indices to propagate to
          for (j in seq(i + 1, min(length(current_treatment_id), i + window), by = 1)) {
            if (is.na(propagated_treatment_id[id_indices[j]])) {
              propagated_treatment_id[id_indices[j]] <- current_treatment_id[i]
            } else {
              break  # Stop if propagation encounters an already-filled treatment_id
            }
          }
        }
      }
    }
  }

  # Ensure output length matches input length
  if (length(propagated_treatment_id) != length(treatment_id)) {
    stop("Length mismatch: propagated_treatment_id and treatment_id must have the same length.")
  }

  return(propagated_treatment_id)
}
