# Simulate discrete version of population dynamic model
# n_sp: number of species
# tmax: number of time points
# lmax: number of locations
# gamma: strength of density regulation
# alpha: inverse spatial scaling
# ss2: species specific response to environmental variation
# sr2: variation in growth rate among species (heterogeneity)
# sE2: general response to environmental variation
# r0: mean growth rate
library(mvtnorm)

f_sim_pop_dyn_ext <- function(n_sp = 100,
                          tmax = 50,
                          lmax = 1,
                          gamma = 0.01,
                          alpha = 0.6,
                          ss2 = 0.02,
                          sr2 = 0.002,
                          sE2 = 0.01,
                          r0 = 0.1){
  
  # Draw growth rate for each species
  r <- rnorm(n_sp, r0, sqrt(sr2))
  # Compute inital log abundance for each species, which is set equal to the 
  # mean log carrying capacity, lnK = r0 / gamma
  x0 <- rep(r0 / gamma, n_sp)
  # Draw general responses to environmental variation for each time step,
  # exponentially correlated among locations
  mat <- dist(cbind(1:lmax, 1:lmax), upper = TRUE, diag = TRUE)
  mat <- exp(-as.matrix(alpha * mat))
  sc <- rmvnorm(n = tmax, mean = rep(0, lmax), sigma = sE2 * mat)
  # Set initial log abundance vector
  res <- list()
  for (j in 1:lmax){
    x <- c()
    x <- cbind(x, x0)
    res[[j]] <- x
  }
  # For each time step:
  for (i in 1:tmax){
    ss <- rmvnorm(n = nrow(x), mean = rep(0, lmax), sigma = ss2 * mat)
    # The next log abundance is
    for (j in 1:lmax){
      x <- cbind(res[[j]],
                 # The previous log abundance
                 res[[j]][, i] +
                   # Plus the density regulated growth rate
                   r - gamma * res[[j]][, i] +
                   # Plus species specific response to environmental variation
                   # (unique for each species)
                   ss[, j] +
                   # Plus general response to environmental variation
                   sc[i, j])
      # If the new log abundance is less than zero, set the log abundance to zero
      # The species is assumed to be extinct and there is no probability of
      # returning to the community.
      x[, i + 1] <- ifelse(x[, i + 1] < 0, 0, x[, i + 1])
      res[[j]] <- x
    }
  }
  for (j in 1:lmax){
    # Name each column as a year
    x <- res[[j]]
    colnames(x) <- paste("y", 0:tmax, sep = "_")
    # Transform the data to tibble format
    x <- as_tibble(x) %>% 
      # Each row is a species
      add_column(species = as.factor(1:nrow(x))) %>% 
      # Long format
      pivot_longer(cols = -species,
                   values_to = "log_abundance",
                   names_to = "year") %>% 
      dplyr::select(-year) %>% 
      # Add new numeric year variable
      group_by(species) %>% 
      mutate(year = 1:length(log_abundance)) %>% 
      ungroup()
    res[[j]] <- x
  }
  # Return simulated log abundances
  return(res)
}
