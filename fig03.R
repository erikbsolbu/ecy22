# Packages used
library(tidyverse)
library(patchwork)

# Figure 3 displays the estimates of population dynamic parameters from
# simulations of community dynamics. These parameters are different depending
# on the strength of density regulation. We plot strength of density
# regulation (gamma), general response to environmental variation (sE2),
# variation in growth rate among species (sr2), and species-specific response
# to environmental variation (ss2).

# We generate true values (tv) to compare with. We consider the case where
# sc2 = 0.1 in the main text.
tv_points <- res_long %>% 
  # Select results from where true value of sc2 is 0.1
  filter(tsc2 == 0.1,
         # Strength of density regulation
         name %in% c("gamma",
                     # Species-specific response to environmental variation
                     expression(sigma[s]^2),
                     # General response to environmental variation
                     expression(sigma[E]^2),
                     # Variation in growth rate among species
                     expression(sigma[r]^2))) %>% 
  # Select true values of se2 and eta in addition
  dplyr::select(tse2, tsc2, tgamma, eta, name) %>% 
  # Need only one value for each combination
  distinct() %>% 
  # Set the true values for each parameter 
  mutate(value = ifelse(
    # Strength of density regulation
    name == "gamma",
    tgamma,
    # Correlation in mean log abundance
    ifelse(name == "gamma[c]",
           tgamma,
           # Species-specific response to environmental variation
           ifelse(name == "sigma[s]^2",
                  tse2 * tgamma * 2,
                  # General response to environmental variation
                  ifelse(name == "sigma[E]^2",
                         tsc2 * (2 * tgamma),
                         # Variation in growth rate among species
                         (1 - tse2) * tgamma ^ 2))))) %>% 
  # Add group differentiating between strengths of density regulation and
  # parameter
  mutate(name2 = paste(ifelse(tgamma == 0.05, "(a)",
                              ifelse(tgamma == 0.1, "(b)",
                                     "(c)")),
                       name, sep = " "))

# Make strength of density regulation group a factor
tv_points$name2 <- as.factor(tv_points$name2)
# Set the levels to differentiate each sub-plot (and one way to make greek
# letters in plot)
levels(tv_points$name2) <- c(expression(paste("(a) ", gamma)),
                             expression(paste("(d) ", sigma[E]^2)),
                             expression(paste("(g) ", sigma[r]^2)),
                             expression(paste("(j) ", sigma[s]^2)),
                             expression(paste("(b) ", gamma)),
                             expression(paste("(e) ", sigma[E]^2)),
                             expression(paste("(h) ", sigma[r]^2)),
                             expression(paste("(k) ", sigma[s]^2)),
                             expression(paste("(c) ", gamma)),
                             expression(paste("(f) ", sigma[E]^2)),
                             expression(paste("(i) ", sigma[r]^2)),
                             expression(paste("(l) ", sigma[s]^2)))

# Sub-set the data in order to make grouping variable
res_long_p1 <- res_long %>% 
  # Common environmental variation sc2 = 0.1
  filter(tsc2 == 0.1,
         # Strength of density regulation
         name %in% c("gamma",
                     # Species-specific response to environmental variation
                     expression(sigma[s]^2),
                     # General response to environmental variation
                     expression(sigma[E]^2),
                     # Variation in growth rate among species
                     expression(sigma[r]^2))) %>% 
  # Make new grouping variable
  mutate(name2 = paste(ifelse(tgamma == 0.05, "(a)",
                              ifelse(tgamma == 0.1, "(b)",
                                     "(c)")), name, sep = " "))

# Make grouping variable as factor
res_long_p1$name2 <- as.factor(res_long_p1$name2)
levels(res_long_p1$name2) <- c(expression(paste("(a) ", gamma)),
                               expression(paste("(d) ", sigma[E]^2)),
                               expression(paste("(g) ", sigma[r]^2)),
                               expression(paste("(j) ", sigma[s]^2)),
                               expression(paste("(b) ", gamma)),
                               expression(paste("(e) ", sigma[E]^2)),
                               expression(paste("(h) ", sigma[r]^2)),
                               expression(paste("(k) ", sigma[s]^2)),
                               expression(paste("(c) ", gamma)),
                               expression(paste("(f) ", sigma[E]^2)),
                               expression(paste("(i) ", sigma[r]^2)),
                               expression(paste("(l) ", sigma[s]^2)))

# Set levels of sampling intensity
levels(res_long_p1$eta) <- c("1%",
                             "10%",
                             "50%")

# Make the figure
fig03 <- res_long_p1 %>% 
  ggplot(aes(x = as.factor(tse2), y = value)) +
  # Box-plot illustrating the variation in parameter estimates
  geom_boxplot(aes(x = as.factor(tse2),
                   y = value,
                   fill = as.factor(eta))) +
  # True values as red stars
  geom_point(data = tv_points,
             colour = "red",
             shape = 8,
             size = 3) +
  # Organize the plot along the new grouping variable
  facet_wrap(~ name2,
             scales = "free_y",
             labeller = label_parsed,
             ncol = 3,
             dir = "v") +
  labs(x = expression(paste("True proportion of ", sigma[e]^2, sep = "")),
       y = "Parameter estimate") +
  scale_fill_brewer("Sampling intensity",
                    direction = -1,
                    palette = 5) +
  # Set theme
  theme_bw() +
  # Legend position
  theme(legend.position = "bottom",
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"))

# Clean-up
rm(list = c("res_long", "res_long_p1", "tv_points"))
