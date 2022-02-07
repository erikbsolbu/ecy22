# Packages used
library(tidyverse)
library(patchwork)
library(sf)
library(glmmTMB)
# Optional packages for parallel computing of bootstrap estimates:
library(foreach)
library(parallel)
library(doParallel)

# This data set is downloaded from https://zenodo.org/record/5026943 where the
# "BioTIMEQuery_24_06_2021.zip" is the file that contains this and many other
# data sets. The STUDY_ID of the fish data is '467' and the following code
# loads the data set (which has been unzipped into a 'biotime' folder and
# selects the data set and then writes this data set back to the working folder
# for easy access later.

# Here is also a direct link to the data set, but note that the code below uses
# the unzipped data file above:
# https://biotime.st-andrews.ac.uk/selectStudy.php?study=467

# un-comment the first time the data has been downloaded
# BioTIMEQuery_24_06_2021 <- read_csv("biotime/BioTIMEQuery_24_06_2021.csv")
# 
# fish_data <- BioTIMEQuery_24_06_2021 %>% 
#   filter(STUDY_ID == "467") %>% 
#   rename(ABUNDANCE = sum.allrawdata.ABUNDANCE,
#          BIOMASS = sum.allrawdata.BIOMASS)
# 
# write.csv(fish_data[, -1], "fish_data.csv", row.names = FALSE)

# Load the saved (much smaller) data set
fish_data <- read_csv("fish_data.csv")

# Transform to utm coordinates for spatial distance calculation
latlong_fish <- distinct(dplyr::select(fish_data, LATITUDE, LONGITUDE))

st_fish <- st_sfc(st_multipoint(cbind(latlong_fish$LONGITUDE,
                                      latlong_fish$LATITUDE)))

st_crs(st_fish) <- "+proj=longlat +datum=WGS84"

st_fish <- st_transform(st_fish, "+proj=utm +zone=29 ellps=WGS84")

utm_fish <- st_coordinates(st_fish)

# Convert distance to kilometers
ll_utm_fish <- as_tibble(cbind(latlong_fish, utm_fish[, -3]/1000))

fish_data <- left_join(fish_data, ll_utm_fish)

# Create spatial and temporal variables used by glmmTMB
fish_data <- fish_data %>%
  mutate(location = numFactor(X, Y),
         time_y = numFactor(YEAR))

# Cleanup
rm(list = c("latlong_fish", "ll_utm_fish", "st_fish", "utm_fish"))

# Wide transformation of the data and fill in zero abundance for each
# unobserved combination of location and time for each species.
fish_data_wide <- fish_data %>% 
  dplyr::select(STUDY_ID, YEAR, time_y, location, GENUS_SPECIES, ABUNDANCE) %>% 
  pivot_wider(names_from = GENUS_SPECIES,
              values_from = ABUNDANCE,
              values_fill = 0)

# Transform back to long format for analysis.
fish_data_long <- fish_data_wide %>% 
  pivot_longer(cols = -c(STUDY_ID:location),
               names_to = "species",
               values_to = "abundance") %>% 
  rename(year = YEAR)

fish_data <- fish_data_long

rm(list = c("fish_data_wide", "fish_data_long"))

# Model fitting ####

# Temporal model

fish_mod01 <- glmmTMB(abundance ~ 
                        # Relative log abundance dynamics:
                        # Within-species variation:
                        ou(time_y + 0 | species:location) +
                        # Among-species variation:
                        (1 | species) +
                        # Mean log abundance dynamics:
                        # Within-location variation:
                        ou(time_y + 0 | location),
                      # Data set:
                      data = fish_data,
                      # Observation process:
                      family = poisson(link = "log"))

## Spatial model

fish_mod02 <- glmmTMB(abundance ~
                        # Relative log abundance dynamics:
                        # Within-species variation:
                        exp(location + 0 | species:time_y) +
                        # Among-species variation:
                        (1 | species) +
                        # Mean log abundance dynamics:
                        # Within-year variation:
                        exp(location + 0 | time_y),
                      # Data set:
                      data = fish_data,
                      # Observation process:
                      family = poisson(link = "log"))

## Parameter uncertainty - bootstrap

# Temporal model #un-comment to run
# boot_fish_est_01 <- c()
# for (i in 1:100){
#   # Simulate new data
#   boot_data_fish <- fish_data %>% 
#     mutate(sim = simulate(fish_mod01)$sim_1)
#   # Re-fit the model
#   boot_fish_mod01 <- glmmTMB(sim ~
#                                ou(time_y + 0 | species:location) +
#                                (1 | species) +
#                                ou(time_y + 0 | location),
#                              data = boot_data_fish,
#                              family = poisson(link = "log"))
#   # Store the bootstrap estimates:
#   boot_fish_est_01 <- rbind(boot_fish_est_01,
#                             c(boot_fish_mod01$fit$par, i))
# }

# Temporal model in parallel
f_sim_and_est_1 <- function(i){
  # Simulate new data
  boot_data_fish <- fish_data %>% 
    mutate(sim = simulate(fish_mod01)$sim_1)
  # Re-fit the model
  boot_fish_mod01 <- glmmTMB(sim ~
                               ou(time_y + 0 | species:location) +
                               (1 | species) +
                               ou(time_y + 0| location),
                             data = boot_data_fish,
                             family = poisson(link = "log"))
  # Return the estimated parameters
  c(boot_fish_mod01$fit$par, i)
}

# How many cores do we have available?
k <- detectCores()

# Use all cores except one
cl <- makeCluster(k - 1)
registerDoParallel(cl)

# Run the bootstrap in parallel (un-comment to run)
# boot_fish_est_01 <- foreach(i = 1:1000,
#                             .combine = "rbind",
#                             .errorhandling = "remove",
#                             .packages = c("tidyverse", "glmmTMB")) %dopar%
#   {f_sim_and_est_1(i)}

# Shut down the workers
stopCluster(cl)

# Spatial model in parallel
f_sim_and_est_2 <- function(i){
  # Simulate new data
  boot_data_fish <- fish_data %>% 
    mutate(sim = simulate(fish_mod02)$sim_1)
  # Re-fit the model
  boot_fish_mod02 <- glmmTMB(sim ~
                               exp(location + 0 | species:time_y) +
                               (1 | species) +
                               exp(location + 0 | time_y),
                             data = boot_data_fish,
                             family = poisson(link = "log"))
  # Return the estimated parameters
  c(boot_fish_mod02$fit$par, i)
}

# How many cores do we have available?
k <- detectCores()

# Use all cores except one
cl <- makeCluster(k - 1)
registerDoParallel(cl)

# Run the bootstrap in parallel (un-comment to run)
# boot_fish_est_02 <- foreach(i = 1:1000,
#                             .combine = "rbind",
#                             .errorhandling = "remove",
#                             .packages = c("tidyverse", "glmmTMB")) %dopar%
#   {f_sim_and_est_2(i)}

# Shut down the workers
stopCluster(cl)

# Save bootstrap for easy access if needed (un-comment to run)
# write_rds(list(boot_fish_est_01,
#                boot_fish_est_02), "boot_fish.rds")
# Load bootstrap if needed
boot <- read_rds("boot_fish.rds")
boot_fish_est_01 <- boot[[1]]
boot_fish_est_02 <- boot[[2]]

# Results ####

## Retrieving estimates

# Join point estimates with bootstrap replicates:
fish_est_01 <- rbind(c(fish_mod01$fit$par, 0),
                     boot_fish_est_01)
fish_est_02 <- rbind(c(fish_mod02$fit$par, 0),
                     boot_fish_est_02)
# Set column names. Note 'rep = 0' are the point estimates.
colnames(fish_est_01) <- c("beta0", "lse2", "lgamma", "lsh2",
                           "lsc2", "lgamma_c", "rep")
colnames(fish_est_02) <- c("beta0", "lse2", "lalphainv", "lsh2", 
                           "lsc2", "lalpha_cinv", "rep")

fish_est_01 <- as_tibble(fish_est_01)
fish_est_02 <- as_tibble(fish_est_02)

# Transform parameters
fish_est_01 <- fish_est_01 %>% 
  mutate(se2 = exp(lse2) ^ 2,
         gamma = exp(lgamma),
         gamma_c = exp(lgamma_c),
         sh2 = exp(lsh2) ^ 2,
         sc2 = exp(lsc2) ^ 2,
         ss2 = se2 * 2 * gamma,
         sr2 = sh2 * gamma ^ 2)

fish_est_02 <- fish_est_02 %>% 
  mutate(se2 = exp(lse2) ^ 2,
         alpha = 1 / exp(lalphainv),
         alpha_c = 1 / exp(lalpha_cinv),
         sh2 = exp(lsh2) ^ 2,
         sc2 = exp(lsc2) ^ 2)

## Summary table

# Select point estimates (rep == 0) for temporal and spatial model
fish_est <- bind_rows(add_column(filter(fish_est_01, rep == 0),
                                 dim = "Temporal"),
                      add_column(filter(fish_est_02, rep == 0),
                                 dim = "Spatial")) %>% 
  # Compute temporal (tr) and spatial (sp) scale, for relative log abundance 
  # and mean log abundance (_c)
  mutate(tr = 1 / gamma,
         sp = 1 / alpha,
         tr_c = 1 / gamma_c,
         sp_c = 1 / alpha_c) %>% 
  dplyr::select(dim, sh2, se2, tr, sp,
                sc2, tr_c, sp_c,
                gamma, ss2, sr2) %>% 
  pivot_longer(sh2:sr2) %>% 
  filter(!is.na(value)) %>%
  # Re-arrange the parameters
  add_column(order = c(1, 3, 5, 7, 9, 11:13, 2, 4, 6, 8, 10)) %>% 
  arrange(order)

# Make confidence intervals for bootstrap estimates (rep != 0)
fish_conf_int <- bind_rows(add_column(filter(fish_est_01, rep != 0),
                                      dim = "Temporal"),
                           add_column(filter(fish_est_02, rep != 0),
                                      dim = "Spatial")) %>% 
  # Compute temporal (tr) and spatial (sp) scale, for relative log abundance 
  # and mean log abundance (_c)
  mutate(tr = 1 / gamma,
         sp = 1 / alpha,
         tr_c = 1 / gamma_c,
         sp_c = 1 / alpha_c) %>% 
  dplyr::select(dim, sh2, se2, tr, sp,
                sc2, tr_c, sp_c,
                gamma, ss2, sr2) %>% 
  pivot_longer(sh2:sr2) %>% 
  filter(!is.na(value)) %>%
  group_by(dim, name) %>% 
  summarise(conf1 = quantile(value, 0.025),
            conf2 = quantile(value, 0.975))

# Make confidence intervals a single variable
fish_res <- left_join(fish_est, fish_conf_int, by = c("dim", "name")) %>% 
  mutate(`95% CI` = paste("(", round(conf1, digits = 2), ", ",
                          round(conf2, digits = 2), ")", sep = ""))

# Add names and organize parameters
fish_res_names <- fish_res %>% 
  dplyr::select(dim, name) %>% 
  add_column(` ` = c("Within samples", "", "", "", "", "",
                     "Among samples", "", "", "",
                     "Dynamic parameters", "", ""),
             Parameter = c("Among species - ecological heterogeneity, $\\sigma_h^2$",
                           "",
                           "Within species - environmental effects, $\\sigma_e^2$",
                           "",
                           "Temporal scale, $1/\\gamma$",
                           "Spatial scale, $1/\\alpha$",
                           "General environment, $\\sigma_c^2$",
                           "",
                           "Temporal scale, $1/\\gamma_c$",
                           "Spatial scale, $1/\\alpha_c$",
                           "Strength of density regulation, $\\gamma$",
                           "Species-specific response to environment, $\\sigma_s^2$",
                           "Variation in growth rate among species, $\\sigma_r^2$"))

fish_res <- left_join(fish_res, fish_res_names, by = c("dim", "name"))

# Keep relevant variables
fish_res_final <- fish_res %>%
  mutate(Estimate = round(value, digits = 2)) %>% 
  dplyr::select(` `, Parameter, Estimate, `95% CI`)

## Figures ####

### Correlation functions

# Temporal correlation in relative log abundance
# Time points to compute correlation for
time = seq(0, max(fish_data$year) - min(fish_data$year), by = 1)
# Correlation function
f_temp_x_cor <- function(data){
  tibble(time = time,
         cor = (data$se2 * exp(-data$gamma * time) + data$sh2) / 
           (data$se2 + data$sh2))
}
# Compute correlation for each replicate
fish_cor_01_x <- fish_est_01 %>% 
  group_by(rep) %>% 
  nest() %>% 
  mutate(cor = map(data, f_temp_x_cor)) %>% 
  dplyr::select(-data) %>% 
  unnest(cor) %>% 
  ungroup()

# Temporal correlation in mean log abundance
# Time points to compute correlation for
time <- seq(0, max(fish_data$year) - min(fish_data$year), by = 1)
# Correlation function
f_temp_xbar_cor <- function(data){
  tibble(time = time,
         cor = exp(-data$gamma_c * time))
}
# Compute correlation for each replicate
fish_cor_01_xbar <- fish_est_01 %>% 
  group_by(rep) %>% 
  nest() %>% 
  mutate(cor = map(data, f_temp_xbar_cor)) %>% 
  dplyr::select(-data) %>% 
  unnest(cor) %>% 
  ungroup()

# Spatial correlation in relative log abundance
# Distances to compute correlation for
distance = seq(0, round(max(dist(parseNumLevels(fish_data$location)))), by = 1)
# Correlation function
f_space_x_cor <- function(data){
  distance = distance
  tibble(distance = distance,
         cor = (data$se2 * exp(- data$alpha * distance) + data$sh2) / 
           (data$se2 + data$sh2))
}
# Compute correlation for each replicate
fish_cor_02_x <- fish_est_02 %>% 
  group_by(rep) %>% 
  nest() %>% 
  mutate(cor = map(data, f_space_x_cor)) %>% 
  dplyr::select(-data) %>% 
  unnest(cor) %>% 
  ungroup()

# Spatial correlation in mean log abundance
# Distances to compute correlation for
distance = seq(0, round(max(dist(parseNumLevels(fish_data$location)))), by = 1)
# Correlation function
f_space_xbar_cor <- function(data){
  distance = distance
  tibble(distance = distance,
         cor = exp(- data$alpha_c * distance))
}
# Compute the correlation for each replicate
fish_cor_02_xbar <- fish_est_02 %>% 
  group_by(rep) %>% 
  nest() %>% 
  mutate(cor = map(data, f_space_xbar_cor)) %>% 
  dplyr::select(-data) %>% 
  unnest(cor) %>% 
  ungroup()

# Temporal correlation in relative log abundance
fish_cor_01_x_sum <- fish_cor_01_x %>% 
  # Skip the initial point estimates
  filter(rep != 0) %>% 
  group_by(time) %>% 
  summarise(q025 = quantile(cor, probs = 0.025),
            q975 = quantile(cor, probs = 0.975))

# Temporal correlation in mean log abundance
fish_cor_01_xbar_sum <- fish_cor_01_xbar %>% 
  # Skip the initial point estimates
  filter(rep != 0) %>% 
  group_by(time) %>% 
  summarise(q025 = quantile(cor, probs = 0.025),
            q975 = quantile(cor, probs = 0.975))

# Spatial correlation in relative log abundance
fish_cor_02_x_sum <- fish_cor_02_x %>% 
  # Skip the initial point estimates
  filter(rep != 0) %>% 
  group_by(distance) %>% 
  summarise(q025 = quantile(cor, probs = 0.025),
            q975 = quantile(cor, probs = 0.975))

# Spatial correlation in mean log abundance
fish_cor_02_xbar_sum <- fish_cor_02_xbar %>% 
  # Skip the initial point estimates
  filter(rep != 0) %>% 
  group_by(distance) %>% 
  summarise(q025 = quantile(cor, probs = 0.025),
            q975 = quantile(cor, probs = 0.975))

# Temporal correlation in relative log abundance
p1 <- fish_cor_01_x_sum %>% 
  ggplot(aes(x = time, y = NULL)) + 
  geom_ribbon(aes(x = time, ymin = q025, ymax = q975), alpha = 0.25) +
  geom_line(data = filter(fish_cor_01_x, rep == 0),
            aes(x = time, y = cor)) +
  lims(y = c(0, 1)) +
  labs(x = "Time (years)", y = "Correlation") +
  scale_x_continuous(breaks = seq(0, 12, by = 3)) +
  theme_bw()

# p1 # un-comment to view the figure separately

# Spatial correlation in relative log abundance
p2 <- fish_cor_02_x_sum %>% 
  ggplot(aes(x = distance, y = NULL)) + 
  geom_ribbon(aes(x = distance, ymin = q025, ymax = q975), alpha = 0.25) +
  geom_line(data = filter(fish_cor_02_x, rep == 0),
            aes(x = distance, y = cor)) +
  lims(y = c(0, 1)) +
  labs(x = "Distance (km)", y = "Correlation") +
  scale_x_continuous(breaks = seq(0, 36, by = 6)) +
  theme_bw()

# p2 # un-comment to view the figure separately

# Temporal correlation in mean log abundance
p3 <- fish_cor_01_xbar_sum %>% 
  ggplot(aes(x = time, y = NULL)) + 
  geom_ribbon(aes(x = time, ymin = q025, ymax = q975), alpha = 0.25) +
  geom_line(data = filter(fish_cor_01_xbar, rep == 0),
            aes(x = time, y = cor)) +
  lims(y = c(0, 1)) +
  labs(x = "Time (years)", y = "Correlation") +
  scale_x_continuous(breaks = seq(0, 12, by = 3)) +
  theme_bw()

# p3 # un-comment to view the figure separately

# Spatial correlation in mean log abundance
p4 <- fish_cor_02_xbar_sum %>% 
  ggplot(aes(x = distance, y = NULL)) + 
  geom_ribbon(aes(x = distance, ymin = q025, ymax = q975), alpha = 0.25) +
  geom_line(data = filter(fish_cor_02_xbar, rep == 0),
            aes(x = distance, y = cor)) +
  lims(y = c(0, 1)) +
  labs(x = "Distance (km)", y = "Correlation") +
  scale_x_continuous(breaks = seq(0, 36, by = 6)) +
  theme_bw()

# p4 # un-comment to view the figure separately

### Total variance and partitioning

# Total variance in the temporal model
tot_var_sum_01 <- fish_est_01 %>% 
  mutate(boot = as.numeric(rep > 0),
         rel_tot_var = se2 + sh2,
         gen_tot_var = sc2,
         prc_rel_se2 = se2 / rel_tot_var,
         prc_rel_sh2 = (sh2) / rel_tot_var) %>% 
  dplyr::select(boot:prc_rel_sh2) %>% 
  pivot_longer(rel_tot_var:prc_rel_sh2) %>% 
  group_by(boot, name) %>% 
  summarise(est = mean(value),
            q025 = quantile(value, probs = 0.025),
            q975 = quantile(value, probs = 0.975)) %>% 
  add_column(dimension = "Temporal") %>% 
  mutate(environment = ifelse(str_detect(name, "rel"), "Species specific", "General"),
         tot_var = ifelse(str_detect(name, "tot"), "Total variance", "Relative"),
         het = ifelse(str_detect(name, "_sh"), "Heterogeneity", "Environmental"))

# Total variance in the spatial model
tot_var_sum_02 <- fish_est_02 %>% 
  mutate(boot = as.numeric(rep > 0),
         rel_tot_var = se2 + sh2,
         gen_tot_var = sc2,
         prc_rel_se2 = se2 / rel_tot_var,
         prc_rel_sh2 = (sh2) / rel_tot_var) %>% 
  dplyr::select(boot:prc_rel_sh2) %>% 
  pivot_longer(rel_tot_var:prc_rel_sh2) %>% 
  group_by(boot, name) %>% 
  summarise(est = mean(value),
            q025 = quantile(value, probs = 0.025),
            q975 = quantile(value, probs = 0.975)) %>% 
  add_column(dimension = "Spatial") %>% 
  mutate(environment = ifelse(str_detect(name, "rel"), "Species specific", "General"),
         tot_var = ifelse(str_detect(name, "tot"), "Total variance", "Relative"),
         het = ifelse(str_detect(name, "_sh"), "Heterogeneity", "Environmental"))

tot_var_sum <- bind_rows(tot_var_sum_01,
                         tot_var_sum_02)

dodge <- position_dodge(width = 0.9)

tot_var <- tot_var_sum %>% 
  filter(tot_var == "Total variance")

p5 <- tot_var %>% 
  ggplot(aes(x = dimension, y = est)) +
  geom_col(data = filter(tot_var,
                         boot == 0),
           aes(linetype = dimension),
           colour = "black", fill = "dark grey") +
  geom_errorbar(data = filter(tot_var,
                              boot == 1),
                aes(ymin = q025, ymax = q975, linetype = dimension),
                position = dodge, width = 0.25) +
  scale_linetype_manual(values = c("Spatial" = 1,
                                   "Temporal" = 1)) +
  labs(x = "Dimension", y = "Total variance") +
  guides(linetype = "none") +
  facet_grid(. ~ environment) + 
  theme_bw()

# p5 # un-comment to view figure separately

dodge <- position_dodge(width = 1)

rel_var <- tot_var_sum %>% 
  filter(tot_var == "Relative")

p6 <- rel_var %>%
  ggplot(aes(x = het, y = est)) + 
  geom_col(data = filter(rel_var,
                         boot == 0),
           aes(fill = het, linetype = dimension),
           colour = "black", position = dodge) +
  geom_errorbar(data = filter(rel_var,
                              boot == 1),
                aes(ymin = q025, ymax = q975, linetype = dimension),
                position = dodge, width = 0.25) +
  scale_fill_manual(values = c("Heterogeneity" = "#8dd3c7",
                               "Environmental" = "#bebada")) +
  scale_x_discrete(labels = c("Env.", "Het.")) +
  scale_linetype_manual(values = c("Spatial" = 1,
                                   "Temporal" = 1)) +
  labs(x = "Variance component", y = "Proportion") +
  lims(y = c(0, 1)) +
  guides(fill = "none",
         linetype = "none") +
  facet_grid(. ~ environment) + 
  theme_bw()

# p6 # un-comment to view figure separately

# Clean-up
rm(list = c("boot", "boot_fish_est_01", "boot_fish_est_02",
            "cl", "dodge", "distance", "k", "time",
            "fish_conf_int",
            "fish_cor_01_x", "fish_cor_01_x_sum",
            "fish_cor_01_xbar", "fish_cor_01_xbar_sum",
            "fish_cor_02_x", "fish_cor_02_x_sum",
            "fish_cor_02_xbar", "fish_cor_02_xbar_sum",
            "fish_data",
            "fish_est", "fish_est_01", "fish_est_02",
            "fish_mod01", "fish_mod02",
            "fish_res", "fish_res_names",
            "rel_var",
            "tot_var", "tot_var_sum", "tot_var_sum_01", "tot_var_sum_02",
            "f_sim_and_est_1", "f_sim_and_est_2",
            "f_space_x_cor", "f_space_xbar_cor",
            "f_temp_x_cor", "f_temp_xbar_cor"))
