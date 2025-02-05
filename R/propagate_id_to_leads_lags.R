# Assign id to leads and lags.
propagate_attack_id <- function(dates, id, attack_id, window) {
  # Ensure input vectors are of the same length
  if (length(dates) != length(attack_id) || length(dates) != length(id)) {
    stop("Dates, id, and attack_id must have the same length.")
  }
  
  # Initialize a vector to store the propagated attack IDs
  propagated_attack_id <- attack_id
  
  # Iterate over each unique `id`
  unique_ids <- unique(id)
  
  for (current_id in unique_ids) {
    # Get indices for the current `id`
    id_indices <- which(id == current_id)
    
    # Extract relevant data for the current `id`
    current_attack_id <- attack_id[id_indices]
    
    # Iterate through each index in the current `id`
    for (i in seq_along(current_attack_id)) {
      if (!is.na(current_attack_id[i])) {  # If the current day has an attack ID assigned
        # Propagate to the previous days (negative leads, for the same id)
        if (i > 1) {  # Ensure there are previous indices to propagate to
          for (j in seq(i - 1, max(1, i - window), by = -1)) {
            if (is.na(propagated_attack_id[id_indices[j]])) {
              propagated_attack_id[id_indices[j]] <- current_attack_id[i]
            } else {
              break  # Stop if propagation encounters an already-filled attack_id
            }
          }
        }
        
        # Propagate to the next days (positive lags, for the same id)
        if (i < length(current_attack_id)) {  # Ensure there are next indices to propagate to
          for (j in seq(i + 1, min(length(current_attack_id), i + window), by = 1)) {
            if (is.na(propagated_attack_id[id_indices[j]])) {
              propagated_attack_id[id_indices[j]] <- current_attack_id[i]
            } else {
              break  # Stop if propagation encounters an already-filled attack_id
            }
          }
        }
      }
    }
  }
  
  # Ensure output length matches input length
  if (length(propagated_attack_id) != length(attack_id)) {
    stop("Length mismatch: propagated_attack_id and attack_id must have the same length.")
  }
  
  return(propagated_attack_id)
}