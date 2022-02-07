# Simulate community dynamics and estimate the parameters, preferably in
# parallel!

f_sim_and_est <- function(i, j, k, l, eta = 0.01){
  
  # Strength of density regulation
  gamma <- i
  # Species-specific response to environmental variation
  ss2 <- j * gamma * 2
  # Among-species variation
  sh2 <- 1 - j
  # Variation in growth rate among species
  sr2 <- sh2 * gamma ^ 2
  # General response to environmental variation
  sE2 <- k * gamma * 2
  # Mean growth rate, to obtain mean log carrying capacity equal to ten
  r0 <- 10 * gamma
  
  # Simulate community dynamics
  sim <- f_sim_pop_dyn(n_sp = 100,
                       tmax = 370,
                       gamma = gamma,
                       ss2 = ss2,
                       sr2 = sr2,
                       sE2 = sE2,
                       r0 = r0)
  
  # From the simulated dynamics of log abundance, we assume a sampling level
  # equal to eta
  # Then we simulate the Poisson sampling process.
  # And transform 'year' to a factor used in glmmTMB
  sim <- sim %>%
    mutate(log_abundance = log_abundance + log(eta),
           abundance = rpois(nrow(sim), exp(log_abundance)),
           time = numFactor(year))
  
  est <- glmmTMB(abundance ~
                   # Relative log-abundance dynamics:
                   # Within-species variation
                   ou(time + 0 | species) +
                   # Among-species variation
                   (1 | species) + 
                   # Mean log-abundance dynamics:
                   # Among-samples variation
                   ou(time + 0| location),
                 # Use the last 50 time points and add location factor for
                 # group when estimating temporal correlation in mean log
                 # abundance
                 data = add_column(filter(sim,
                                          year > 321),
                                   location = 1),
                 # Poisson observation process
                 family = poisson(link = "log"))
  
  # Return the estimates and parameter configuration
  c(est$fit$par, i, j, k, l)
  
}
