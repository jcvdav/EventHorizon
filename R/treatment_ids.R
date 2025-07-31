#' Identify treatment ids based on a user-defined window
#'
#' @description
#' If in treatment falls within the window, it is considered a single treatment event and a new treatment id is created.
#'
#' @param id A column name containing the unique identifier of the observational unit in the panel (the i dimension)
#' @param time A column name containing the time variable in the panel (i.e. the t dimension)
#' @param treatment A column name containing the name of the dummy variable indicating whether unit i is treated at time t
#' @param window A number indicating the number of periods within which two or more treatment events are merged. This should be in the same units as `time`
#' @param time_units A character vector specifying the units in which `time` is measured. Default is "days", other options are "weeks", "months", or "years"
#'
#' @returns A character vector specifying the new treatment ids
#'
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
#'                                     window = 3))
#'
#' head(event_horizon_panel)
#'
treatment_ids <- function(id, time, treatment, window, time_units = "days") {
  # Initialize columns
  treatment_id <- rep(NA, length(treatment))  # This will store initial treatment IDs

  # Counter for unique treatment IDs
  treatment_counter <- 0
  group_anchor <- NA  # Track the anchor date of the current group
  processed_indices <- rep(FALSE, length(treatment))  # Track processed indices
  current_id <- NA  # Initialize current_id

  # Iterate over the treatment vector to assign initial treatment IDs
  for (i in seq_along(treatment)) {
    # Reset counter when encountering a new id
    if (!is.na(id[i]) && (is.na(current_id) || id[i] != current_id)) {
      current_id <- id[i]
      treatment_counter <- 0
    }

    if (!is.na(treatment[i]) && treatment[i] && !processed_indices[i]) {
      # Start a new group
      treatment_counter <- treatment_counter + 1
      group_anchor <- time[i]
      treatment_id[i] <- paste0("treatment_", id[i], "_", treatment_counter)  # Assign treatment ID
      processed_indices[i] <- TRUE

      # Check for treatments in the following days within the window
      for (j in (i + 1):length(treatment)) {
        # Stop propagation if we reach a different id
        if (!is.na(id[j]) && id[j] != id[i]) break

        if (j > length(treatment) || is.na(treatment[j]) || processed_indices[j]) next

        days_diff <- as.numeric(difftime(time[j], group_anchor, units = time_units))

        # If there's an treatment within the window, propagate it
        if (days_diff <= window * 2 && treatment[j]) {
          treatment_id[j] <- treatment_id[i]  # Propagate treatment ID
          processed_indices[j] <- TRUE
          group_anchor <- time[j]  # Update anchor to the latest treatment date
        }

        # If no treatment within the window, stop propagation
        if (days_diff > window * 2) break
      }
    }
  }

  # Now propagate treatment_id where needed, using the same logic
  for (i in 2:length(treatment_id)) {
    if (is.na(treatment_id[i]) && !is.na(treatment_id[i - 1])) {  # If current cell is NA and previous is not
      for (j in i:length(treatment_id)) {
        if (!is.na(treatment_id[j]) && treatment_id[j] == treatment_id[i - 1]) {
          # Fill all cells in between with the same treatment_id
          treatment_id[i:j] <- treatment_id[i - 1]
          break
        } else if (!is.na(treatment_id[j]) && treatment_id[j] != treatment_id[i - 1]) {
          # Stop when the next treatment_id is different
          break
        }
      }
    }
  }

  # Return the final treatment ID vector
  return(treatment_id)
}
