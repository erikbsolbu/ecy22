# Packages used
library(tidyverse)

# Load simulation results
res_01 <- read_rds("sim_example_res_01.rds") # eta = 0.01
res_02 <- read_rds("sim_example_res_02.rds") # eta = 0.1
res_03 <- read_rds("sim_example_res_03.rds") # eta = 0.5

# Add column names
colnames(res_01) <- c("mu", "lse", "lgamma", "lsh", "lsc", "lgamma_c", "tgamma", "tse2", "tsc2", "rep")
colnames(res_02) <- c("mu", "lse", "lgamma", "lsh", "lsc", "lgamma_c", "tgamma", "tse2", "tsc2", "rep")
colnames(res_03) <- c("mu", "lse", "lgamma", "lsh", "lsc", "lgamma_c", "tgamma", "tse2", "tsc2", "rep")

# Add sampling intensity (eta)
res_01 <- as_tibble(res_01) %>% 
  add_column(eta = 0.01)
res_02 <- as_tibble(res_02) %>% 
  add_column(eta = 0.1)
res_03 <- as_tibble(res_03) %>% 
  add_column(eta = 0.5)

# Join results in common table
res <- bind_rows(res_01,
                 res_02,
                 res_03) %>%
  # Transform variables
  mutate(
    # Within-species variation
    se2 = exp(lse) ^ 2,
    # Strength of density regulation
    gamma = exp(lgamma),
    # Correlation parameter for mean log abundance
    gamma_c = exp(lgamma_c),
    # Among-species variation
    sh2 = exp(lsh) ^ 2,
    # Ratio of environmental variation in relative log abundance
    rse2 = se2 / (se2 + sh2),
    # Within-samples variation
    sc2 = exp(lsc) ^ 2,
    # Genera response to environmental variation
    sE2 = sc2 * (gamma_c * 2),
    # Species-specific response to environmental variation
    ss2 = se2 * gamma * 2,
    # Variation in growth rate among species
    sr2 = sh2 * gamma ^ 2)

# Transform table to long format
res_long <- res %>% 
  pivot_longer(cols = c(mu, se2:sr2))

# Make parameter names factor
res_long$name <- as.factor(res_long$name)
# Set levels of factor (one way of making greek letters in plot)
levels(res_long$name) <- c("gamma",
                           expression(gamma[c]),
                           "mu",
                           expression(sigma[e]^2/(sigma[e]^2+sigma[h]^2)),
                           expression(sigma[c]^2),
                           expression(sigma[e]^2),
                           expression(sigma[E]^2),
                           expression(sigma[h]^2),
                           expression(sigma[r]^2),
                           expression(sigma[s]^2))

# Make sampling intensity factor
res_long$eta <- as.factor(res_long$eta)
# Set levels of factor
levels(res_long$eta) <- c(expression(paste("Sampling ", "1%", sep = "")),
                          expression(paste("Sampling ", "10%", sep = "")),
                          expression(paste("Sampling ", "50%", sep = "")))

# Clean up
rm(list = c("res", "res_01", "res_02", "res_03"))
