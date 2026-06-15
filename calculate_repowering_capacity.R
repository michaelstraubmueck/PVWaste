calculate_repowering_capacity <- function(pof_t, net_capacity) {
  # Ensure inputs are numeric vectors
  net_capacity <- as.numeric(unlist(net_capacity))
  pof_t <- as.numeric(unlist(pof_t))
  
  # Initialize vectors for repowering demand and gross capacity additions
  repowering_capacity <- numeric(length(net_capacity))
  gross_capacity <- numeric(length(net_capacity))
  
  # Calculate repowering demand iteratively
  for (t in 1:length(net_capacity)) {
    repowering_capacity_t <- numeric(t)
    
    if (t > 1) {
      for (i in 1:(t - 1)) {
        repowering_capacity_t[i] <- pof_t[t - i] * gross_capacity[i]
      }
    }
    
    # Gross capacity equals net capacity plus replacement capacity
    repowering_capacity[t] <- sum(repowering_capacity_t, na.rm = TRUE)
    gross_capacity[t] <- net_capacity[t] + repowering_capacity[t]
  }

  # Return net, repowering, and gross capacity additions  
  result <- data.frame(
    net_capacity = net_capacity,
    repowering_capacity = repowering_capacity,
    gross_capacity = gross_capacity
  )
  
  return(result)
}
