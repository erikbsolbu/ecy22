# Packages used
library(tidyverse)
library(patchwork)

# Figure 2 displays the estimates of variance components of the GLMM based on
# simulations of community dynamics, that is, within- and among-species
# variation, in addition to variation in the common mean value among years.

# We generate true values (tv) to compare with. We consider the case where
# sc2 = 0.1 in the main text.
tv_points <- res_long %>% 
  # Select results from where true value of sc2 is 0.1
  filter(tsc2 == 0.1,
         # Mean log abundance
         name %in% c(#"mu",
                     # variation in common mean among years
                     expression(sigma[c]^2),
                     # within-species variation
                     expression(sigma[e]^2),
                     # among-species variation
                     expression(sigma[h]^2))) %>% 
  # Select true vales of se2, gamma and eta in addition
  dplyr::select(tse2, tsc2, tgamma, eta, name) %>%
  # Need only one value for each combination
  distinct() %>% 
  # Set the true values for each parameter
  mutate(
    # True proportion of environmental variation in relative log abundance
    value = ifelse(name == "sigma[e]^2/(sigma[e]^2 + sigma[h]^2)",
                   tse2,
                   # Within-species variation
                   ifelse(name == "sigma[e]^2",
                          tse2,
                          # Among-species variation
                          ifelse(name == "sigma[h]^2",
                                 1 - tse2,
                                 # Common environmental variation
                                 ifelse(name == "sigma[c]^2",
                                        tsc2,
                                        # Mean log abundance depends on
                                        # sampling intensity
                                        ifelse(eta == levels(res_long$eta)[1],
                                               # If eta = 0.01
                                               10 + log(0.01),
                                               ifelse(eta == levels(res_long$eta)[2],
                                                      # If eta = 0.1
                                                      10 + log(0.1),
                                                      # If eta = 0.5
                                                      10 + log(0.5))))))))

# Make the figure
fig02 <- res_long %>% 
  # Select results from where true value of sc2 is 0.1
  filter(tsc2 == 0.1,
         # Target parameters (explained above)
         name %in% c(#"mu", 
                     expression(sigma[c]^2),
                     expression(sigma[e]^2),
                     expression(sigma[h]^2))) %>%
  ggplot(aes(x = as.factor(tse2), y = value)) +
  # Box-plots illustrating the variation in parameter estimates
  geom_boxplot(aes(x = as.factor(tse2),
                   y = value,
                   fill = as.factor(tgamma))) +
  # True values as red stars
  geom_point(data = tv_points,
             colour = "steelblue",
             shape = 4,
             size = 3,
             stroke = 1.1) +
  # Organize in gird
  facet_grid(name ~ eta,
             scales = "free_y",
             labeller = label_parsed) +
  labs(x = expression(paste("True proportion of ", sigma[e]^2, sep = "")),
       y = "Parameter estimate") +
  scale_fill_brewer("Strength of density regulation",
                    direction = -1,
                    palette = 7) +
  # Set theme
  theme_bw() +
  # Legend position
  theme(legend.position = "bottom")

# Clean up
rm(list = c("tv_points"))