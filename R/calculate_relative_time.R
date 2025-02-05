# Assign relative times before and after treatement window. Treatement window gets 0
calculate_relative_time_rle <- function(dates, id, attack_id, window) {
  # Initialize relative time vector
  relative_time <- rep(NA, length(attack_id))
  
  # Iterate over each unique `id`
  unique_ids <- unique(id)
  
  for (current_id in unique_ids) {
    # Get indices for the current `id`
    id_indices <- which(id == current_id)
    
    # Extract relevant data for the current `id`
    current_attack_id <- attack_id[id_indices]
    current_dates <- dates[id_indices]
    
    # Identify attack periods (TRUE where attack_id is not NA)
    events <- rle(!is.na(current_attack_id))
    
    # Initialize the starting index for the current `id`
    current_index <- 1
    
    for (i in seq_along(events$values)) {
      run_length <- events$lengths[i]
      
      if (events$values[i]) {  # If this is an attack period (TRUE)
        start_index <- current_index
        end_index <- current_index + run_length - 1
        
        # Assign 0 during the attack period
        relative_time[id_indices[start_index:end_index]] <- 0
        
        # Days before the attack (within the window)
        if (start_index > 1) {
          pre_event_length <- min(window, start_index - 1)
          relative_time[id_indices[(start_index - pre_event_length):(start_index - 1)]] <- 
            -pre_event_length:-1
        }
        
        # Days after the attack (within the window)
        if (end_index < length(current_attack_id)) {
          post_event_length <- min(window, length(current_attack_id) - end_index)
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