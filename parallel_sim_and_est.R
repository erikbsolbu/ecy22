# Required packages for parallel computing
library(foreach)
library(parallel)
library(doParallel)

# Make an index of all parameter combinations

index <- c()

# Strength of density regulation (gamma)
for (i in c(0.05, 0.1, 0.5)){
  
  # Among-species variation (se2)
  for (j in seq(0.2, 0.8, by = 0.2)){
    
    # Among-samples variation (sc2)
    for (k in c(0.01, 0.1)){
      
      # Number of replicates (for testing)
      for (l in 1:10){
      # Number of replicates (will take a long time)
      # for (l in 1:1000){
      
        index <- rbind(index,
                       c(i, j, k, l))
        
      }
    }
  }
}

nrow(index) # total number of iterations

# How many cores do we have available?
cores <- detectCores()
# Use all cores except one
cl <- makeCluster(cores - 1)
registerDoParallel(cl)

# Run the simulation and estimation in parallel (eta = 0.01)
res_01 <- foreach(i = index[, 1],
                  j = index[, 2],
                  k = index[, 3],
                  l = index[, 4],
                  .combine = "rbind",
                  .packages = c("dplyr", "tidyr", "tibble", "glmmTMB"),
                  .errorhandling = "remove") %dopar%
  {f_sim_and_est(i, j, k, l, eta = 0.01)}

# Run the simulation and estimation in parallel (eta = 0.1)
res_02 <- foreach(i = index[, 1],
                  j = index[, 2],
                  k = index[, 3],
                  l = index[, 4],
                  .combine = "rbind",
                  .packages = c("dplyr", "tidyr", "tibble", "glmmTMB"),
                  .errorhandling = "remove") %dopar%
  {f_sim_and_est(i, j, k, l, eta = 0.1)}

# Run the simulation and estimation in parallel (eta = 0.5)
res_03 <- foreach(i = index[, 1],
                  j = index[, 2],
                  k = index[, 3],
                  l = index[, 4],
                  .combine = "rbind",
                  .packages = c("dplyr", "tidyr", "tibble", "glmmTMB"),
                  .errorhandling = "remove") %dopar%
  {f_sim_and_est(i, j, k, l, eta = 0.5)}

# Shut down the workers
stopCluster(cl)

# Save all the hard work:
write_rds(res_01, "sim_example_res_01.rds") # eta = 0.01
write_rds(res_02, "sim_example_res_02.rds") # eta = 0.1
write_rds(res_03, "sim_example_res_03.rds") # eta = 0.5

# Clean-up
rm(list = c("index", "cl", "cores", "i", "j", "k", "l"))