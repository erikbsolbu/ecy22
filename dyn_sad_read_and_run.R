# R-code for "Analyzing dynamic species abundance distributions using
# generalized linear mixed models"

# Part 1: Population dynamics and GLMM theory ####

# Figure 1: Population dynamics, covariance structure and simulated data.

# Part a: dynamics of each variance component.

source("fig01a.R")

# Part b: covariance structure of random effects.

source("fig01b.R")

# Part c: sample simulations of population dynamics.

source("fig01c.R")

# Assemble all the plots into a single figure, using the 'patchwork' package
library(patchwork)

(p_dyn_a + p_dyn_b + p_dyn_c + p_dyn_d + 
    plot_layout(nrow = 1, byrow = TRUE)) /
  (p_heterogeneity + p_common + p_species + p_observation +
     plot_layout(nrow = 1, byrow = TRUE)) /
  sim_plot +
  plot_layout(heights = c(1, 1, 1.2))

# Save the figure
# ggsave("Fig1.tiff", height = 7.25, width = 8.75, dpi = 800)
ggsave("Fig1.jpg", height = 7.25, width = 8.75, dpi = 800)
# ggsave("Fig1.pdf", height = 7.25, width = 8.75, dpi = 800)

# Part 2: Estimation of variance components and population dynamic parameters ####

# In this section we simulate community dynamics over time and try to estimate
# the parameters using GLMM for a range of different dynamic parameters. To 
# assess the estimation uncertainty, we simulate and estimate 1000 replicates
# for each parameter configuration. This will take quite some time, so we run
# the procedure in parallel, but it will still require many hours to compute.
# It is therefore suggested to run with few replicates first and add additional
# replicates if interested.

# Load function
source("f_sim_and_est.R")

# The following runs the simulation and estimation function. This takes a long
# time. Preferably you have a computer with many cores, otherwise it is 
# recommended to run for a small number of iterations, to get an impression of
# the range of estimates and if time allows it, repeat the runs and store each
# result separately and join them in the end.

# Run simulation and estimation
# Do not run if you are in a hurry, and something else in the mean time!
# source("parallel_sim_and_est.R") # un-comment to run it
# We have included our own simulations and estimates to make the figures in the
# main text.

# Data preparation
source("sim_and_est_res_prep.R")

# Figure 2: estimates of variance components from simulated community dynamics
source("fig02.R")

# Plot the figure
fig02
# Save figure 
# ggsave("Fig2.tiff", height = 8, width = 6, dpi = 800)
ggsave("Fig2.jpg", height = 6, width = 6, dpi = 800)
# ggsave("Fig2.pdf", height = 6, width = 6, dpi = 800)

# Figure 3: estimates of population dynamic parameters from simulated data
source("fig03.R")

# Plot the figure
fig03
# Save figure 
# ggsave("Fig3.tiff", height = 8, width = 6, dpi = 800)
ggsave("Fig3.jpg", height = 8, width = 6, dpi = 800)
# ggsave("Fig3.pdf", height = 8, width = 6, dpi = 800)

# Part 3: Case study 1: 'fish data'

# This case study is described in detail in the vignette "Analyzing dynamic
# Species Abundance Distributions using GLMM" in a separate file 
# "dyn_sad_vignette.Rmd". The following file contains the R-code from this
# vignette, except the table and figure are rendered below.

# Note: the next R-file is assuming the bootstrap estimates have already been
# run and stored. Un-comment the necessary code to re-run the bootstrap.

source("case_study_fish.R")

# Render table
knitr::kable(fish_res_final)

# Plot the figure
(((p1 + p2) / (p3 | p4)) / (p5 | p6)) + plot_annotation(tag_levels = "a")
# Save figure
# ggsave("Fig4.tiff", height = 7, width = 7, dpi = 800)
ggsave("Fig4.jpg", height = 7, width = 7, dpi = 800)
# ggsave("Fig4.pdf", height = 7, width = 7, dpi = 800)

# Part 4: Case study 2: 'bats data'

# Note: the next R-file is assuming the bootstrap estimates have already been
# run and stored. Un-comment the necessary code to re-run the bootstrap.

source("case_study_bats.R")

# Render tables
knitr::kable(bats_res_final)
knitr::kable(bats_res_final_a)
knitr::kable(bats_res_final_b)

# Plot the figure
(((p1 / p2 / p3) / p4)) + plot_annotation(tag_levels = "a")
# Save figure
# ggsave("Fig5.tiff", height = 8, width = 6, dpi = 800)
ggsave("Fig5.jpg", height = 8.75, width = 8.75, dpi = 800)
# ggsave("Fig5.pdf", height = 8.75, width = 8.75, dpi = 800)
