calculate_pv_waste <- function(pof_t, amount, first_year, last_year) {
  # Initialize matrix to store annual waste amounts
  waste_amount <- matrix(0, nrow = length(pof_t), ncol = 1)
  
  # Calculate waste generation for each year based on past installations
  # and the probability of failure in each age period
  for (t in (length(pof_t) + 1):2) {
    h <- t - 1
    waste_amount_t <- numeric(h)
    
    for (i in 1:h) {
      waste_amount_t[i] <- pof_t[t - i] * amount[i]
    }
    
    waste_amount[h, 1] <- sum(waste_amount_t,na.rm = TRUE)
  }
  
  # Return results as a data frame with years and annual PV waste
  result <- data.frame(
    Year = seq(first_year + 2, last_year + 1, 1),
    pv_waste = waste_amount[, 1]
  )
  
  return(result)
}

