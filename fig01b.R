# Required packages
library(tidyverse)

# Create variables
var1 <- factor(rep(paste(rep(paste("X_", 1:2, sep = ""),
                             each = 5),
                         "(",
                         rep(paste("t_", 1:5, sep = ""),
                             2),
                         ")", sep = ""),
                   each = 10),
               levels = paste(rep(paste("X_", 1:2, sep = ""),
                                  each = 5),
                              "(",
                              rep(paste("t_", 1:5, sep = ""),
                                  2),
                              ")", sep = ""))

var2 <- factor(rep(paste(rep(paste("X_", 1:2, sep = ""),
                             each = 5),
                         "(",
                         rep(paste("t_", 1:5, sep = ""),
                             2),
                         ")", sep = ""),
                   10),
               levels = paste(rep(paste("X_", 2:1, sep = ""),
                                  each = 5),
                              "(",
                              rep(paste("t_", 5:1, sep = ""),
                                  2),
                              ")", sep = ""))

# Table indicating correlation due to species heterogeneity
tbl_heterogeneity <- tibble(value = c(rep(c(1, NA), each = 5),
                                      rep(c(1, NA), each = 5),
                                      rep(c(1, NA), each = 5),
                                      rep(c(1, NA), each = 5),
                                      rep(c(1, NA), each = 5),
                                      rep(c(NA, 1), each = 5),
                                      rep(c(NA, 1), each = 5),
                                      rep(c(NA, 1), each = 5),
                                      rep(c(NA, 1), each = 5),
                                      rep(c(NA, 1), each = 5)),
                            var1 = var1,
                            var2 = var2) %>% 
  add_column(case = "(e) - Heterogeneity")

p_heterogeneity <- tbl_heterogeneity %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_tile(aes(fill = value), color = "black") +
  scale_fill_gradient(low = "#8dd3c7", 
                      high = "#8dd3c7",
                      na.value = "#8dd3c700") +
  labs(x = "Species i, time t", y = "Species i, time t") +
  guides(fill = "none") +
  scale_y_discrete(labels = paste(rep(2:1, each = 5), rep(5:1, 2), sep = ",")) +
  scale_x_discrete(labels = paste(rep(1:2, each = 5), rep(1:5, 2), sep = ",")) +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")) +
  facet_wrap(~ case)

# Table indicating correlation due to common environmental noise
tbl_common <- tibble(value = c(rep(c(1, 0.8, 0.6, 0.4, 0.2), 2),
                               rep(c(0.8, 1, 0.8, 0.6, 0.4), 2),
                               rep(c(0.6, 0.8, 1, 0.8, 0.6), 2),
                               rep(c(0.4, 0.6, 0.8, 1, 0.8), 2),
                               rep(c(0.2, 0.4, 0.6, 0.8, 1), 2),
                               rep(c(1, 0.8, 0.6, 0.4, 0.2), 2),
                               rep(c(0.8, 1, 0.8, 0.6, 0.4), 2),
                               rep(c(0.6, 0.8, 1, 0.8, 0.6), 2),
                               rep(c(0.4, 0.6, 0.8, 1, 0.8), 2),
                               rep(c(0.2, 0.4, 0.6, 0.8, 1), 2)),
                     var1 = var1,
                     var2 = var2) %>% 
  add_column(case = "(f) - Common")

p_common <- tbl_common %>% 
  ggplot(aes(x = var1, y = var2)) +
  geom_tile(aes(fill = value), color = "black") +
  scale_fill_gradient(low = "#ffffb330",
                      high = "#ffffb3",
                      na.value = "#ffffb300") +
  labs(x = "Species i, time t", y = "") +
  guides(fill = "none") +
  scale_x_discrete(labels = paste(rep(1:2, each = 5), rep(1:5, 2), sep = ",")) +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")) +
  facet_wrap(~ case)

# Table indicating correlation due to species specific environmental noise
tbl_environmental <- tibble(value = c(c(1, 0.8, 0.6, 0.4, 0.2), rep(NA, 5),
                                c(0.8, 1, 0.8, 0.6, 0.4), rep(NA, 5),
                                c(0.6, 0.8, 1, 0.8, 0.6), rep(NA, 5),
                                c(0.4, 0.6, 0.8, 1, 0.8), rep(NA, 5),
                                c(0.2, 0.4, 0.6, 0.8, 1), rep(NA, 5),
                                rep(NA, 5), c(1, 0.8, 0.6, 0.4, 0.2),
                                rep(NA, 5), c(0.8, 1, 0.8, 0.6, 0.4),
                                rep(NA, 5), c(0.6, 0.8, 1, 0.8, 0.6),
                                rep(NA, 5), c(0.4, 0.6, 0.8, 1, 0.8),
                                rep(NA, 5), c(0.2, 0.4, 0.6, 0.8, 1)),
                      var1 = var1,
                      var2 = var2) %>% 
  add_column(case = "(g) - Environmental")

p_species <- tbl_environmental %>% 
  ggplot(aes(x = var1, y = var2)) +
  geom_tile(aes(fill = value), color = "black") +
  scale_fill_gradient(low = "#bebada30",
                      high = "#bebada",
                      na.value = "#bebada00") +
  labs(x = "Species i, time t", y = "") +
  guides(fill = "none") +
  scale_x_discrete(labels = paste(rep(1:2, each = 5), rep(1:5, 2), sep = ",")) +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(), #) +
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")) +
  facet_wrap(~ case)

# Table indicating correlation due to observation noise
tbl_observation <- tibble(value = c(1, rep(NA, 9),
                                    NA, 1, rep(NA, 8),
                                    NA, NA, 1, rep(NA, 7),
                                    rep(NA, 3), 1, rep(NA, 6),
                                    rep(NA, 4), 1, rep(NA, 5),
                                    rep(NA, 5), 1, rep(NA, 4),
                                    rep(NA, 6), 1, rep(NA, 3),
                                    rep(NA, 7), 1, rep(NA, 2),
                                    rep(NA, 8), 1, rep(NA, 1),
                                    rep(NA, 9), 1),
                          var1 = var1,
                          var2 = var2) %>% 
  add_column(case = "(h) - Observation")

p_observation <- tbl_observation %>% 
  ggplot(aes(x = var1, y = var2)) +
  geom_tile(aes(fill = value), color = "black") +
  scale_fill_gradient(low = "#fb8072",
                      high = "#fb8072",
                      na.value = "#fb807200") +
  labs(x = "Species i, time t", y = "") +
  guides(fill = "none") +
  scale_x_discrete(labels = paste(rep(1:2, each = 5), rep(1:5, 2), sep = ",")) +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
  plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")) +
  facet_wrap(~ case)

# Clean up
rm(list = c("var1", "var2", "tbl_heterogeneity", "tbl_common", "tbl_environmental", "tbl_observation"))