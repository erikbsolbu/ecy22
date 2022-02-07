# Simulate discrete version of population dynamic model
# n_sp: number of species
# tmax: number of time points
# gamma: strength of density regulation
# ss2: species specific response to environmental variation
# sr2: variation in growth rate among species (heterogeneity)
# sE2: general response to environmental variation
# r0: mean growth rate

f_sim_pop_dyn <- function(n_sp = 100,
                          tmax = 50,
                          gamma = 0.01,
                          ss2 = 0.06,
                          sr2 = 0.00001,
                          sE2 = 0.04,
                          r0 = 0.1){
  
  # Draw growth rate for each species
  r <- rnorm(n_sp, r0, sqrt(sr2))
  # Compute inital log abundance for each species, which is set equal to the 
  # mean log carrying capacity, lnK = r0 / gamma
  x0 <- rep(r0 / gamma, n_sp)
  # Draw general responses to environmental variation for each time step
  sc <- rnorm(tmax, 0, sqrt(sE2))
  # Set initial log abundance vector
  x <- c()
  x <- cbind(x, x0)
  # For each time step:
  for (i in 1:tmax){
    # The next log abundance is
    x <- cbind(x,
               # The previous log abunadnce
               x[, i] +
                 # Plus the density regulated growth rate
                 r - gamma * x[, i] +
                 # Plus species specific response to environmental variation
                 # (unique for each species)
                 rnorm(nrow(x), 0, sqrt(ss2)) +
                 # Plus general response to environmental variation
                 sc[i])
    # If the new log abundance is less than zero, set the log abundance to zero
    # The species is assumed to be extinct and there is no probability of
    # returning to the community.
    x[, i + 1] <- ifelse(x[, i + 1] < 0, 0, x[, i + 1])
  }
  # Name each column as a year
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
  # Return simulated log abundances
  return(x)
}
