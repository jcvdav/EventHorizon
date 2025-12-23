# Assign relative times before and after treatement window. Treatement window gets 0
#' Title
#'
#' @param id A column name containing the unique identifier of the observational unit in the panel (the i dimension)
#' @param treatment_id A character specifying the treatment id, as produced by `treatment_ids()`
#' @param window A number indicating the number of periods within which two or more treatment events are merged
#'
#' @returns A numeric vector containing the relative time of treatment in number of periods before or after the start and end of treatment ids identified by `treatment_ids()`
#' @export
#'
#' @seealso [treatment_ids()]
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
#'                                                window = 3))
#'
#' head(event_horizon_panel)
#'
#
calculate_relative_time <- function(id, treatment_id, window) {
  # Initialize relative time vector
  relative_time <- rep(NA, length(treatment_id))

  # Iterate over each unique `id`
  unique_ids <- unique(id)

  for (current_id in unique_ids) {
    # Get indices for the current `id`
    id_indices <- which(id == current_id)

    # Extract relevant data for the current `id`
    current_treatment_id <- treatment_id[id_indices]

    # Identify treatment periods (TRUE where treatment_id is not NA)
    events <- rle(!is.na(current_treatment_id))

    # Initialize the starting index for the current `id`
    current_index <- 1

    for (i in seq_along(events$values)) {
      run_length <- events$lengths[i]

      if (events$values[i]) {  # If this is a treatment period (TRUE)
        start_index <- current_index
        end_index <- current_index + run_length - 1

        # Assign 0 during the treatment period
        relative_time[id_indices[start_index:end_index]] <- 0

        # Days before the treatment (within the window)
        if (start_index > 1) {
          pre_event_length <- min(window, start_index - 1)
          relative_time[id_indices[(start_index - pre_event_length):(start_index - 1)]] <-
            -pre_event_length:-1
        }

        # Days after the treatment (within the window)
        if (end_index < length(current_treatment_id)) {
          post_event_length <- min(window, length(current_treatment_id) - end_index)
          relative_time[id_indices[(end_index + 1):(end_index + post_event_length)]] <-
            1:post_event_length
        }
      }

      # Move the index forward by the length of this run
      current_index <- current_index + run_length
    }
  }

  return(relative_time)
}
