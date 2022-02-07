# Load simulation function
source("f_sim_pop_dyn.R")

# First permutation of parameters:
# Low environmental species specific environmental variation and
# weak density regulation

# Within-species variation
se2 <- 0.2
# Among-species variation
sh2 <- 1 - se2
# Variation in mean log abundance
sc2 <- 0.1
# Strength of density regulation
gamma <- 0.05
# Species-specific response to environmental variation
ss2 <- se2 * gamma * 2
# Variation in growth rate among species
sr2 <- sh2 * gamma ^ 2
# General response to environmental variation
sE2 <- sc2 * gamma * 2
# Mean growth rate set so that mean log carrying capacity is equalt to ten
r0 <- 10 * gamma

# Simulation
sim_dyn_111 <- f_sim_pop_dyn(n_sp = 100,
                              tmax = 370,
                              gamma = gamma,
                              ss2 = ss2,
                              sr2 = sr2,
                              sE2 = sE2,
                              r0 = r0)


# Second permutation of parameters
# High environmental species specific environmental variation and
# weak density regulation
# See first permutation for explanation
se2 <- 0.8
sh2 <- 1 - se2
sc2 <- 0.1
gamma <- 0.05
ss2 <- se2 * gamma * 2
sr2 <- sh2 * gamma ^ 2
sE2 <- sc2 * gamma * 2
r0 <- 10 * gamma

# Simulation
sim_dyn_112 <- f_sim_pop_dyn(n_sp = 100,
                              tmax = 370,
                              gamma = gamma,
                              ss2 = ss2,
                              sr2 = sr2,
                              sE2 = sE2,
                              r0 = r0)

# Third permutation of parameters
# Low environmental species specific environmental variation and
# medium density regulation
# See first permutation for explanation
se2 <- 0.2
sh2 <- 1 - se2
sc2 <- 0.1
gamma <- 0.1
ss2 <- se2 * gamma * 2
sr2 <- sh2 * gamma ^ 2
sE2 <- sc2 * gamma * 2
r0 <- 10 * gamma

# Simulation
sim_dyn_121 <- f_sim_pop_dyn(n_sp = 100,
                              tmax = 370,
                              gamma = gamma,
                              ss2 = ss2,
                              sr2 = sr2,
                              sE2 = sE2,
                              r0 = r0)

# Fourth permutation of parameters
# High environmental species specific environmental variation and
# medium density regulation
# See first permutation for explanation
se2 <- 0.8
sh2 <- 1 - se2
sc2 <- 0.1
gamma <- 0.1
ss2 <- se2 * gamma * 2
sr2 <- sh2 * gamma ^ 2
sE2 <- sc2 * gamma * 2
r0 <- 10 * gamma

# Simulation
sim_dyn_122 <- f_sim_pop_dyn(n_sp = 100,
                              tmax = 370,
                              gamma = gamma,
                              ss2 = ss2,
                              sr2 = sr2,
                              sE2 = sE2,
                              r0 = r0)

# Fifth permutation of parameters
# Low environmental species specific environmental variation and
# strong density regulation
# See first permutation for explanation
se2 <- 0.2
sh2 <- 1 - se2
sc2 <- 0.1
gamma <- 0.5
ss2 <- se2 * gamma * 2
sr2 <- sh2 * gamma ^ 2
sE2 <- sc2 * gamma * 2
r0 <- 10 * gamma

# Simulation
sim_dyn_211 <- f_sim_pop_dyn(n_sp = 100,
                              tmax = 370,
                              gamma = gamma,
                              ss2 = ss2,
                              sr2 = sr2,
                              sE2 = sE2,
                              r0 = r0)

# Sixth permutation of parameters
# High environmental species specific environmental variation and
# strong density regulation
# See first permutation for explanation
se2 <- 0.8
sh2 <- 1 - se2
sc2 <- 0.1
gamma <- 0.5
ss2 <- se2 * gamma * 2
sr2 <- sh2 * gamma ^ 2
sE2 <- sc2 * gamma * 2
r0 <- 10 * gamma

# Simulation
sim_dyn_212 <- f_sim_pop_dyn(n_sp = 100,
                              tmax = 370,
                              gamma = gamma,
                              ss2 = ss2,
                              sr2 = sr2,
                              sE2 = sE2,
                              r0 = r0)

# Join simulations together
sim_dyn <- bind_rows(add_column(sim_dyn_111, gamma = "(i) ~ gamma == 0.05", se2 = "(1) ~ sigma[e]^2 == 0.2"),
                     add_column(sim_dyn_112, gamma = "(i) ~ gamma == 0.05", se2 = "(2) ~ sigma[e]^2 == 0.8"),
                     add_column(sim_dyn_121, gamma = "(j) ~ gamma == 0.1", se2 = "(1) ~ sigma[e]^2 == 0.2"),
                     add_column(sim_dyn_122, gamma = "(j) ~ gamma == 0.1", se2 = "(2) ~ sigma[e]^2 == 0.8"),
                     add_column(sim_dyn_211, gamma = "(k) ~ gamma == 0.5", se2 = "(1) ~ sigma[e]^2 == 0.2"),
                     add_column(sim_dyn_212, gamma = "(k) ~ gamma == 0.5", se2 = "(2) ~ sigma[e]^2 == 0.8")) %>% 
  filter(year > 321) %>% 
  mutate(year = year - 320)

# Make a plot of simulated community dynamics
sim_plot <- sim_dyn %>% 
  ggplot(aes(x = year, y = log_abundance)) + 
  geom_line(aes(group = species), colour = "grey", alpha = 0.5) +
  geom_line(data = filter(sim_dyn,
                          species %in% 1:2),
            aes(group = species)) +
  theme_bw() +
  labs(x = "Time", y = "Log abundance") +
  facet_grid(se2 ~ gamma, labeller = label_parsed)

# Clean-up
rm(list = c("sim_dyn",
            "sim_dyn_111", "sim_dyn_112", "sim_dyn_121",
            "sim_dyn_122", "sim_dyn_211", "sim_dyn_212",
            "gamma", "r0", "sc2", "se2", "sE2", "sh2", "sr2", "ss2"))
