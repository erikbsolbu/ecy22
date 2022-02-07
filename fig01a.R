# Required packages
library(tidyverse)
library(mvtnorm)

# Set seed for consistency in order to highlight different species
set.seed(19)

# First we simulate among species variation, by drawing one mean value
# for each species
beta0 <- 2
sh2 <- 1
h_i <- rnorm(10, beta0, sqrt(sh2))

# Data used in the first subfigure
fig_data_1 <- tibble(species = as.factor(1:10),
                     h_i = h_i)

# Second we simulate among year variation, drawing one value for each year,
# with a correlation among years
sc2 <- 0.01
# Not the most efficient way to crease a correlation matrix, but it works.
mat <- diag(5)
tmp <- c()
for (i in 1:4){
  tmp <- c(tmp, c(i:1))}
mat[upper.tri(mat)] <- exp(-0.2 * tmp)
tmp <- c()
for (i in 4:1){
  tmp <- c(tmp, c(1:i))}
mat[lower.tri(mat)] <- exp(-0.2 * tmp)

c_t <- rmvnorm(1, mean = rep(0, 5), sigma = sc2 * mat)

# Data used in the second subfigure
fig_data_2 <- tibble(species = rep(as.factor(1:10), 5),
                     h_i = rep(h_i, 5),
                     time = rep(1:5, each = 10),
                     c_t = rep(c_t, each = 10))

# Third, within species variation, one value for each species and each year,
# correlated among years within each species.
ss2 <- 0.1
mat <- diag(5)
tmp <- c()
for (i in 1:4){
  tmp <- c(tmp, c(i:1))}
mat[upper.tri(mat)] <- exp(-0.2 * tmp)
tmp <- c()
for (i in 4:1){
  tmp <- c(tmp, c(1:i))}
mat[lower.tri(mat)] <- exp(-0.2 * tmp)

s_it <- rmvnorm(10, mean = rep(0, 5), sigma = ss2 * mat)

# Data used in the third subfigure
fig_data_3 <- tibble(species = rep(as.factor(1:10), 5),
                     h_i = rep(h_i, 5),
                     time = rep(1:5, each = 10),
                     c_t = rep(c_t, each = 10),
                     s_it = c(s_it))

# Fourth, observation level random noise, independent values for each species
# and year
so2 <- 0.01
s_ijt <- rnorm(10 * 5, mean = 0, sd = sqrt(so2))

# Data used in the fourth subfigure
fig_data_4 <- tibble(species = rep(as.factor(1:10), 5),
                     h_i = rep(h_i, 5),
                     time = rep(1:5, each = 10),
                     c_t = rep(c_t, each = 10),
                     s_it = c(s_it),
                     s_ijt = s_ijt)

# Join subfigure data
fig_data <- bind_rows(add_column(fig_data_1, case = "(a) - Heterogeneity"),
                      add_column(fig_data_2, case = "(b) - Common"),
                      add_column(fig_data_3, case = "(c) - Environmental"),
                      add_column(fig_data_4, case = "(d) - Observation")) %>% 
  mutate(case = factor(case, levels = c("(a) - Heterogeneity",
                                        "(b) - Common",
                                        "(c) - Environmental",
                                        "(d) - Observation")))

p_dyn_a <- fig_data %>% 
  # Select appropriate data
  filter(case == "(a) - Heterogeneity") %>% 
  ggplot(aes()) +
  # Illustrate a 95% probability interval around the mean value
  geom_rect(data = tibble(xmin = c(0.5),
                          xmax = c(5.5),
                          ymin = rep(beta0 - 1.96 * sqrt(sh2), 1),
                          ymax = rep(beta0 + 1.96 * sqrt(sh2), 1),
                          case = "(a) - Heterogeneity"),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#8dd3c7",
            linetype = 2, colour = "black") +
  # Sample values of mean log carrying capacity
  geom_hline(data = filter(fig_data,
                           case == "(a) - Heterogeneity"),
             aes(yintercept = h_i, group = species), alpha = 0.2) +
  # Highlight two species
  geom_hline(data = filter(fig_data,
                           species %in% c("1", "5"),
                           case == "(a) - Heterogeneity"),
             aes(yintercept = h_i, group = species)) +
  coord_cartesian(xlim = c(0.5, 5.5), ylim = c(0, 4)) +
  scale_y_continuous(labels = rep("", 5)) +
  labs(x = "Time", y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),# ) +
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")) +
  facet_wrap(~ case, nrow = 1)

p_dyn_b <- fig_data %>% 
  # Select appropriate data
  filter(case == "(b) - Common") %>% 
  ggplot(aes()) +
  # The previous probability interval in the background
  geom_rect(data = tibble(xmin = c(0.5),
                          xmax = c(5.5),
                          ymin = rep(beta0 - 1.96 * sqrt(sh2), 1),
                          ymax = rep(beta0 + 1.96 * sqrt(sh2), 1),
                          case = "(b) - Common"),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#8dd3c7",
            alpha = 0.2) +
  # Add probability interval around the mean value of the two highlighted
  # species the first year
  geom_rect(data = tibble(xmin = rep(c(0.5) + c(0:4), each = 10),
                          xmax = rep(c(1.5) + c(0:4), each = 10),
                          ymin = rep(h_i - 1.96 * sqrt(sc2), 5),
                          ymax = rep(h_i + 1.96 * sqrt(sc2), 5),
                          case = rep("(b) - Common",
                                     50))[c(seq(1, 41, by = 10)[1],
                                            seq(5, 45, by = 10)[1]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#ffffb3",
            linetype = 2, colour = "black", alpha = 0.9) +
  # Add probability interval around the mean value of the two highlighted
  # species the second year with lighter shade
  geom_rect(data = tibble(xmin = rep(c(0.5) + c(0:4), each = 10),
                          xmax = rep(c(1.5) + c(0:4), each = 10),
                          ymin = rep(h_i - 1.96 * sqrt(sc2), 5),
                          ymax = rep(h_i + 1.96 * sqrt(sc2), 5),
                          case = rep("(b) - Common",
                                     50))[c(seq(1, 41, by = 10)[2],
                                            seq(5, 45, by = 10)[2]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#ffffb3",
            linetype = 2, colour = "black", alpha = 0.8) +
  # Add probability interval around the mean value of the two highlighted
  # species the third year with lighter shade
  geom_rect(data = tibble(xmin = rep(c(0.5) + c(0:4), each = 10),
                          xmax = rep(c(1.5) + c(0:4), each = 10),
                          ymin = rep(h_i - 1.96 * sqrt(sc2), 5),
                          ymax = rep(h_i + 1.96 * sqrt(sc2), 5),
                          case = rep("(b) - Common",
                                     50))[c(seq(1, 41, by = 10)[3],
                                            seq(5, 45, by = 10)[3]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#ffffb3",
            linetype = 2, colour = "black", alpha = 0.6) +
  # Add probability interval around the mean value of the two highlighted
  # species the fourth year with lighter shade
  geom_rect(data = tibble(xmin = rep(c(0.5) + c(0:4), each = 10),
                          xmax = rep(c(1.5) + c(0:4), each = 10),
                          ymin = rep(h_i - 1.96 * sqrt(sc2), 5),
                          ymax = rep(h_i + 1.96 * sqrt(sc2), 5),
                          case = rep("(b) - Common",
                                     50))[c(seq(1, 41, by = 10)[4],
                                            seq(5, 45, by = 10)[4]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#ffffb3",
            linetype = 2, colour = "black", alpha = 0.4) +
  # Add probability interval around the mean value of the two highlighted
  # species the fifth year with lighter shade
  geom_rect(data = tibble(xmin = rep(c(0.5) + c(0:4), each = 10),
                          xmax = rep(c(1.5) + c(0:4), each = 10),
                          ymin = rep(h_i - 1.96 * sqrt(sc2), 5),
                          ymax = rep(h_i + 1.96 * sqrt(sc2), 5),
                          case = rep("(b) - Common",
                                     50))[c(seq(1, 41, by = 10)[5],
                                            seq(5, 45, by = 10)[5]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#ffffb3",
            linetype = 2, colour = "black", alpha = 0.2) +
  # Highlight two species (from subfigure a)
  geom_hline(data = filter(fig_data, species %in% c("1", "5"),
                           case == "(b) - Common"),
             aes(yintercept = h_i, group = species), alpha = 0.2) +
  # Highlight each species dynamics
  geom_line(data = filter(fig_data,
                          case == "(b) - Common"),
            aes(x = time, y = h_i + c_t, group = species), alpha = 0.2) +
  # Highlight two species (point)
  geom_point(data = filter(fig_data, species %in% c("1", "5"),
                           case == "(b) - Common"),
             aes(x = time, y = h_i + c_t)) +
  coord_cartesian(xlim = c(0.5, 5.5), ylim = c(0, 4)) +
  scale_y_continuous(labels = rep("", 5)) +
  labs(x = "Time", y = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")) +
  facet_wrap(~ case, nrow = 1)

p_dyn_c <- fig_data %>% 
  # Select appropriate data
  filter(case == "(c) - Environmental") %>% 
  ggplot(aes()) +
  # The previous probability interval in the background
  geom_rect(data = tibble(xmin = c(0.5),
                          xmax = c(5.5),
                          ymin = rep(beta0 - 1.96 * sqrt(sh2), 1),
                          ymax = rep(beta0 + 1.96 * sqrt(sh2), 1),
                          case = rep("(c) - Environmental", 1)),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#8dd3c7", alpha = 0.2) +
  # The previous probability interval in the background
  geom_rect(data = tibble(xmin = rep(c(0.5), 10),
                          xmax = rep(c(5.5), 10),
                          ymin = rep(h_i - 1.96 * sqrt(sc2), 1),
                          ymax = rep(h_i + 1.96 * sqrt(sc2), 1),
                          case = rep("(c) - Environmental", 10))[c(1, 5), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#ffffb3", alpha = 0.2) +
  # Add probability interval around the mean value of the two highlighted
  # species the first year
  geom_rect(data = tibble(xmin = rep(0.5 + c(0:4), each = 10),
                          xmax = rep(1.5 + c(0:4), each = 10),
                          ymin = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i +
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t -
                                       1.96 * sqrt(ss2), 1),
                          ymax = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i +
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t +
                                       1.96 * sqrt(ss2), 1),
                          case = rep("(c) - Environmental",
                                     50))[c(seq(1, 41, by = 10)[1],
                                            seq(5, 45, by = 10)[1]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#bebada", linetype = 2, col = "black", alpha = 0.9) +
  # Add probability interval around the mean value of the two highlighted
  # species the second year with lighter shade
  geom_rect(data = tibble(xmin = rep(0.5 + c(0:4), each = 10),
                          xmax = rep(1.5 + c(0:4), each = 10),
                          ymin = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i +
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t - 
                                       1.96 * sqrt(ss2), 1),
                          ymax = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i +
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t +
                                       1.96 * sqrt(ss2), 1),
                          case = rep("(c) - Environmental", 
                                     50))[c(seq(1, 41, by = 10)[2],
                                            seq(5, 45, by = 10)[2]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#bebada", linetype = 2, col = "black", alpha = 0.8) +
  # Add probability interval around the mean value of the two highlighted
  # species the third year with lighter shade
  geom_rect(data = tibble(xmin = rep(0.5 + c(0:4), each = 10),
                          xmax = rep(1.5 + c(0:4), each = 10),
                          ymin = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i +
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t -
                                       1.96 * sqrt(ss2), 1),
                          ymax = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i +
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t +
                                       1.96 * sqrt(ss2), 1),
                          case = rep("(c) - Environmental",
                                     50))[c(seq(1, 41, by = 10)[3],
                                            seq(5, 45, by = 10)[3]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#bebada", linetype = 2, col = "black", alpha = 0.6) +
  # Add probability interval around the mean value of the two highlighted
  # species the fourth year with lighter shade
  geom_rect(data = tibble(xmin = rep(0.5 + c(0:4), each = 10),
                          xmax = rep(1.5 + c(0:4), each = 10),
                          ymin = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i + 
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t - 
                                       1.96 * sqrt(ss2), 1),
                          ymax = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i +
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t +
                                       1.96 * sqrt(ss2), 1),
                          case = rep("(c) - Environmental",
                                     50))[c(seq(1, 41, by = 10)[4], 
                                            seq(5, 45, by = 10)[4]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#bebada", linetype = 2, col = "black", alpha = 0.4) +
  # Add probability interval around the mean value of the two highlighted
  # species the fifth year with lighter shade
  geom_rect(data = tibble(xmin = rep(0.5 + c(0:4), each = 10),
                          xmax = rep(1.5 + c(0:4), each = 10),
                          ymin = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i +
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t -
                                       1.96 * sqrt(ss2), 1),
                          ymax = rep(filter(fig_data,
                                            case == "(c) - Environmental")$h_i +
                                       filter(fig_data,
                                              case == "(c) - Environmental")$c_t +
                                       1.96 * sqrt(ss2), 1),
                          case = rep("(c) - Environmental",
                                     50))[c(seq(1, 41, by = 10)[5],
                                            seq(5, 45, by = 10)[5]), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#bebada", linetype = 2, col = "black", alpha = 0.2) +
  # Highlight two species (from subfigure b)
  geom_hline(data = filter(fig_data, species %in% c("1", "5"),
                           case == "(c) - Environmental"),
             aes(yintercept = h_i, group = species), alpha = 0.2) +
  # Highlight two species (from subfigure b)
  geom_point(data = filter(fig_data, species %in% c("1", "5"),
                           case == "(c) - Environmental"),
             aes(x = time, y = h_i + c_t), alpha = 0.2) +
  # Highlight each species dynamics
  geom_line(data = filter(fig_data,
                          case == "(c) - Environmental"),
            aes(x = time, y = h_i + c_t + s_it, group = species), alpha = 0.2) +
  # Highlight two species (point)
  geom_point(data = filter(fig_data, species %in% c("1", "5"),
                           case == "(c) - Environmental"),
             aes(x = time, y = h_i + c_t + s_it)) +
  coord_cartesian(xlim = c(0.5, 5.5), ylim = c(0, 4)) +
  scale_y_continuous(labels = rep("", 5)) +
  labs(x = "Time", y = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")) +
  facet_wrap(~ case, nrow = 1)

p_dyn_d <- fig_data %>% 
  # Select appropriate data
  filter(case == "(d) - Observation") %>% 
  ggplot(aes()) +
  # The previous probability interval in the background
  geom_rect(data = tibble(xmin = c(0.5),
                          xmax = c(5.5),
                          ymin = rep(beta0 - 1.96 * sqrt(sh2), 1),
                          ymax = rep(beta0 + 1.96 * sqrt(sh2), 1),
                          case = "(d) - Observation"),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#8dd3c720", alpha = 0.2) +
  # The previous probability interval in the background
  geom_rect(data = tibble(xmin = rep(c(0.5), 10),
                          xmax = rep(c(5.5), 10),
                          ymin = rep(h_i - 1.96 * sqrt(sc2), 1),
                          ymax = rep(h_i + 1.96 * sqrt(sc2), 1),
                          case = rep("(d) - Observation", 10))[c(1, 5), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#ffffb3", alpha = 0.2) +
  # Add probability interval around the mean value of the first highlighted
  # species
  geom_rect(data = tibble(xmin = rep(0.5 + c(0:4), each = 10),
                          xmax = rep(1.5 + c(0:4), each = 10),
                          ymin = rep(filter(fig_data,
                                            case == "(d) - Observation")$h_i +
                                       filter(fig_data,
                                              case == "(d) - Observation")$c_t -
                                       1.96 * sqrt(ss2), 1),
                          ymax = rep(filter(fig_data,
                                            case == "(d) - Observation")$h_i + 
                                       filter(fig_data,
                                              case == "(d) - Observation")$c_t +
                                       1.96 * sqrt(ss2), 1),
                          case = rep("(d) - Observation", 
                                     50))[c(seq(1, 41, by = 10),
                                            seq(5, 45, by = 10)), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#bebada",
            alpha = 0.2) +
  # Add probability interval around the mean value of the first highlighted
  # species
  geom_rect(data = tibble(xmin = rep(0.5 + c(0:4), each = 10),
                          xmax = rep(1.5 + c(0:4), each = 10),
                          ymin = rep(filter(fig_data,
                                            case == "(d) - Observation")$h_i +
                                       filter(fig_data,
                                              case == "(d) - Observation")$c_t +
                                       filter(fig_data,
                                              case == "(d) - Observation")$s_it -
                                       1.96 * sqrt(so2), 1),
                          ymax = rep(filter(fig_data,
                                            case == "(d) - Observation")$h_i +
                                       filter(fig_data,
                                              case == "(d) - Observation")$c_t +
                                       filter(fig_data,
                                              case == "(d) - Observation")$s_it +
                                       1.96 * sqrt(so2), 1),
                          case = rep("(d) - Observation",
                                     50))[c(seq(1, 41, by = 10),
                                            seq(5, 45, by = 10)), ],
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#fb8072", linetype = 2, col = "black") +
  # Highlight two species (from subfigure c)
  geom_hline(data = filter(fig_data, species %in% c("1", "5"),
                           case == "(d) - Observation"),
             aes(yintercept = h_i, group = species), alpha = 0.2) +
  # Highlight each species dynamics
  geom_line(data = filter(fig_data,
                          case == "(d) - Observation"),
            aes(x = time, y = h_i + c_t + s_it + s_ijt, group = species), alpha = 0.2) +
  # Highlight two species (from subfigure c)
  geom_point(data = filter(fig_data, species %in% c("1", "5"),
                           case == "(d) - Observation"),
             aes(x = time, y = h_i + c_t + s_it), alpha = 0.2) +
  # Highlight two species (point)
  geom_point(data = filter(fig_data, species %in% c("1", "5"),
                           case == "(d) - Observation"),
             aes(x = time, y = h_i + c_t + s_it + s_ijt)) +
  coord_cartesian(xlim = c(0.5, 5.5), ylim = c(0, 4)) +
  scale_y_continuous(labels = rep("", 5)) +
  labs(x = "Time", y = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")) +
  facet_wrap(~ case, nrow = 1)

# Clean up
rm(list = c("c_t", "fig_data", "fig_data_1", "fig_data_2", "fig_data_3",
            "fig_data_4", "mat", "s_it", "beta0", "h_i", "i", "s_ijt",
            "sc2", "sh2", "so2", "ss2", "tmp"))