# Identify treatment windows. If in attack falls within the window, it is considered a single attack
identify_and_populate_attacks <- function(id, dates, attack, window) {
  # Initialize columns
  attack_id <- rep(NA, length(attack))  # This will store initial attack IDs
  
  # Counter for unique attack IDs
  attack_counter <- 0
  group_anchor <- NA  # Track the anchor date of the current group
  processed_indices <- rep(FALSE, length(attack))  # Track processed indices
  current_id <- NA  # Initialize current_id
  
  # Iterate over the attack vector to assign initial attack IDs
  for (i in seq_along(attack)) {
    # Reset counter when encountering a new id
    if (!is.na(id[i]) && (is.na(current_id) || id[i] != current_id)) {
      current_id <- id[i]
      attack_counter <- 0
    }
    
    if (!is.na(attack[i]) && attack[i] && !processed_indices[i]) {
      # Start a new group
      attack_counter <- attack_counter + 1
      group_anchor <- dates[i]
      attack_id[i] <- paste0("attack_", id[i], "_", attack_counter)  # Assign attack ID
      processed_indices[i] <- TRUE
      
      # Check for attacks in the following days within the window
      for (j in (i + 1):length(attack)) {
        # Stop propagation if we reach a different id
        if (!is.na(id[j]) && id[j] != id[i]) break
        
        if (j > length(attack) || is.na(attack[j]) || processed_indices[j]) next
        
        days_diff <- as.numeric(difftime(dates[j], group_anchor, units = "days"))
        
        # If there's an attack within the window, propagate it
        if (days_diff <= window * 2 && attack[j]) {
          attack_id[j] <- attack_id[i]  # Propagate attack ID
          processed_indices[j] <- TRUE
          group_anchor <- dates[j]  # Update anchor to the latest attack date
        }
        
        # If no attack within the window, stop propagation
        if (days_diff > window * 2) break
      }
    }
  }
  
  # Now propagate attack_id where needed, using the same logic
  for (i in 2:length(attack_id)) {
    if (is.na(attack_id[i]) && !is.na(attack_id[i - 1])) {  # If current cell is NA and previous is not
      for (j in i:length(attack_id)) {
        if (!is.na(attack_id[j]) && attack_id[j] == attack_id[i - 1]) {
          # Fill all cells in between with the same attack_id
          attack_id[i:j] <- attack_id[i - 1]
          break
        } else if (!is.na(attack_id[j]) && attack_id[j] != attack_id[i - 1]) {
          # Stop when the next attack_id is different
          break
        }
      }
    }
  }
  
  # Return the final attack ID vector
  return(attack_id)
}