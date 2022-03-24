# Simulate community dynamics and estimate the parameters, preferably in
# parallel!

f_sim_and_est_ext <- function(i, j, k, l, eta = 0.01){
  
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
  sim <- f_sim_pop_dyn_ext(n_sp = 100,
                       tmax = 370,
                       lmax = 50,
                       gamma = gamma,
                       alpha = 0.6,
                       ss2 = ss2,
                       sr2 = sr2,
                       sE2 = sE2,
                       r0 = r0)
  
  # From the simulated dynamics of log abundance, we assume a sampling level
  # equal to eta
  # Then we simulate the Poisson sampling process.
  big_sim <- c()
  # And transform 'year' to a factor used in glmmTMB
  for (lind in 1:50){
    sim_tmp <- sim[[lind]] %>%
      mutate(log_abundance = log_abundance + log(eta),
             abundance = rpois(nrow(sim[[lind]]), exp(log_abundance)),
             time = numFactor(year),
             lx = lind,
             ly = lind)
    big_sim <- bind_rows(big_sim, sim_tmp)
  }
  
  big_sim <- big_sim %>% 
    mutate(location = numFactor(lx, ly))
  
  filter(big_sim,
         year > 321,
         lx <= 10) %>% 
    ggplot(aes(x = time, y = log(abundance))) + 
    geom_line(aes(group = species, colour = species)) + 
    facet_wrap(~ location) +
    guides(colour = "none")
  
  filter(big_sim,
         year > 361) %>% 
    ggplot(aes(x = location, y = log(abundance))) + 
    geom_line(aes(group = species, colour = species)) + 
    facet_wrap(~ time) +
    guides(colour = "none")
  
  est_time <- glmmTMB(abundance ~
                   # Relative log-abundance dynamics:
                   # Within-species variation
                   ou(time + 0 | species:location) +
                   # Among-species variation
                   (1 | species) + 
                   # Mean log-abundance dynamics:
                   # Among-samples variation
                   ou(time + 0| location),
                 # Use the last 50 time points and add location factor for
                 # group when estimating temporal correlation in mean log
                 # abundance
                 data = filter(big_sim,
                               year > 321,
                               lx <= 10),
                 # Poisson observation process
                 family = poisson(link = "log"))
  
  est_loc <- glmmTMB(abundance ~
                   # Relative log-abundance dynamics:
                   # Within-species variation
                   exp(location + 0 | species:time) +
                   # Among-species variation
                   (1 | species) + 
                   # Mean log-abundance dynamics:
                   # Among-samples variation
                   exp(location + 0| time),
                 # Use the last 50 time points and add location factor for
                 # group when estimating temporal correlation in mean log
                 # abundance
                 data = filter(big_sim,
                               year > 361),
                 # Poisson observation process
                 family = poisson(link = "log"))
  
  # Return the estimates and parameter configuration
  rbind(c(est_time$fit$par, i, j, k, l, 1),
        c(est_loc$fit$par, i, j, k, l, 2))
  
}
