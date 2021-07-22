# Monte Carlo simulations of the ACP (Ganter 2021)
# Author: Flavien Ganter
# Created on December 1, 2019; last modified on July 16, 2021



#### PRELIMINARIES ####

# Clear workin space
rm(list = ls())

# Set working directory
setwd("") ### specify path to root replication folder

# Packages and functions
library(foreach)
library(doParallel)
source("Functions/conjacp.R")

# Seed
set.seed(1319)

# Register number of cores
registerDoParallel(cores = 4)



#### SET SIMULATION PARAMETERS ####

# Sample size
n           <- 2000

# Continuous attribute
cont        <- .001
cont_g      <- .001

# Dichotomique attribute (completely independently randomized)
dich12      <- .1
dich12_g    <- -.1

# Categorical attributes (conditionally independently randomized)
cata12      <- -.05
cata13      <- -.12
cata14      <- -.1
cata23      <- -.03
cata24      <- -.1
cata34      <- .01
catb12      <- .1
catb12_123  <- .1
catb12_4    <- -.1
catb12_1234 <- 0
catb12_4123 <- -.05
catb13      <- -.08
catb14      <- -.1
catb23      <- 0
catb24      <- -.04
catb34      <- -.06
cata12_g    <- 0
cata13_g    <- .05
cata14_g    <- -.03
cata23_g    <- -.03
cata24_g    <- .1
cata34_g    <- 0
catb12_g    <- .1
catb13_g    <- -.08
catb14_g    <- .02
catb23_g    <- -.05
catb24_g    <- -.1
catb34_g    <- -.03




#### SIMULATIONS - WITHOUT INTERACTION ####

### This chunk of code takes time to finish. To directly obtain the tables,
### skip to the TABLES section to load previously simulated data.

# Number of simulations
n_simul <- 5000

# Begin loop
simulation_results <- foreach(icount(n_simul)) %dopar% {

  
  ## Simulate data
  
  # Simulate attributes
  # The simulated sample size is four times the size of the expected sample size because
  # observations with forbidden combinations will be removed, and because we want two samples,
  # one for each subgroup.
  data_simul <- data.frame(cont_1 = round(runif(4 * n, 18, 65)),
                           cont_2 = round(runif(4 * n, 18, 65)),
                           dich_1 = sample(c(1,2), 4 * n, replace = TRUE),
                           dich_2 = sample(c(1,2), 4 * n, replace = TRUE),
                           cata_1 = sample(c(1:4), 4 * n, replace = TRUE),
                           cata_2 = sample(c(1:4), 4 * n, replace = TRUE),
                           catb_1 = sample(c(1:4), 4 * n, replace = TRUE),
                           catb_2 = sample(c(1:4), 4 * n, replace = TRUE))
  
  # Remove forbidden combinations
  data_simul <- data_simul[!((data_simul$cata_1 == 4 &
                                data_simul$catb_1 %in% c(3,4)) |
                               (data_simul$cata_2 == 4 &
                                  data_simul$catb_2 %in% c(3,4))),]
  
  # Keep 2*n observations
  data_simul <- data_simul[c(1:(2 * n)),]
  
  # Assign subgroup
  data_simul$group <- c(rep(1, 1000), rep(2, 1000))
  
  # Convert attributes into factor variables
  data_simul[,-c(1,2)] <- lapply(data_simul[,-c(1,2)], as.factor)
  
  # Calculate contrasting selection probability
  data_simul$probability <-
    ifelse(data_simul$group == "1",
           .5 + cont * (data_simul$cont_1 - data_simul$cont_2) +
             dich12 * (data_simul$dich_1 == "1" & data_simul$dich_2 == "2") -
             dich12 * (data_simul$dich_1 == "2" & data_simul$dich_2 == "1") +
             cata12 * (data_simul$cata_1 == "1" & data_simul$cata_2 == "2") -
             cata12 * (data_simul$cata_1 == "2" & data_simul$cata_2 == "1") +
             cata13 * (data_simul$cata_1 == "1" & data_simul$cata_2 == "3") -
             cata13 * (data_simul$cata_1 == "3" & data_simul$cata_2 == "1") +
             cata14 * (data_simul$cata_1 == "1" & data_simul$cata_2 == "4") -
             cata14 * (data_simul$cata_1 == "4" & data_simul$cata_2 == "1") +
             cata23 * (data_simul$cata_1 == "2" & data_simul$cata_2 == "3") -
             cata23 * (data_simul$cata_1 == "3" & data_simul$cata_2 == "2") +
             cata24 * (data_simul$cata_1 == "2" & data_simul$cata_2 == "4") -
             cata24 * (data_simul$cata_1 == "4" & data_simul$cata_2 == "2") +
             cata34 * (data_simul$cata_1 == "3" & data_simul$cata_2 == "4") -
             cata34 * (data_simul$cata_1 == "4" & data_simul$cata_2 == "3") +
             catb12 * (data_simul$catb_1 == "1" & data_simul$catb_2 == "2") -
             catb12 * (data_simul$catb_1 == "2" & data_simul$catb_2 == "1") +
             catb13 * (data_simul$catb_1 == "1" & data_simul$catb_2 == "3") -
             catb13 * (data_simul$catb_1 == "3" & data_simul$catb_2 == "1") +
             catb14 * (data_simul$catb_1 == "1" & data_simul$catb_2 == "4") -
             catb14 * (data_simul$catb_1 == "4" & data_simul$catb_2 == "1") +
             catb23 * (data_simul$catb_1 == "2" & data_simul$catb_2 == "3") -
             catb23 * (data_simul$catb_1 == "3" & data_simul$catb_2 == "2") +
             catb24 * (data_simul$catb_1 == "2" & data_simul$catb_2 == "4") -
             catb24 * (data_simul$catb_1 == "4" & data_simul$catb_2 == "2") +
             catb34 * (data_simul$catb_1 == "3" & data_simul$catb_2 == "4") -
             catb34 * (data_simul$catb_1 == "4" & data_simul$catb_2 == "3"),
           .5 + cont_g * (data_simul$cont_1 - data_simul$cont_2) +
             dich12_g * (data_simul$dich_1 == "1" & data_simul$dich_2 == "2") -
             dich12_g * (data_simul$dich_1 == "2" & data_simul$dich_2 == "1") +
             cata12_g * (data_simul$cata_1 == "1" & data_simul$cata_2 == "2") -
             cata12_g * (data_simul$cata_1 == "2" & data_simul$cata_2 == "1") +
             cata13_g * (data_simul$cata_1 == "1" & data_simul$cata_2 == "3") -
             cata13_g * (data_simul$cata_1 == "3" & data_simul$cata_2 == "1") +
             cata14_g * (data_simul$cata_1 == "1" & data_simul$cata_2 == "4") -
             cata14_g * (data_simul$cata_1 == "4" & data_simul$cata_2 == "1") +
             cata23_g * (data_simul$cata_1 == "2" & data_simul$cata_2 == "3") -
             cata23_g * (data_simul$cata_1 == "3" & data_simul$cata_2 == "2") +
             cata24_g * (data_simul$cata_1 == "2" & data_simul$cata_2 == "4") -
             cata24_g * (data_simul$cata_1 == "4" & data_simul$cata_2 == "2") +
             cata34_g * (data_simul$cata_1 == "3" & data_simul$cata_2 == "4") -
             cata34_g * (data_simul$cata_1 == "4" & data_simul$cata_2 == "3") +
             catb12_g * (data_simul$catb_1 == "1" & data_simul$catb_2 == "2") -
             catb12_g * (data_simul$catb_1 == "2" & data_simul$catb_2 == "1") +
             catb13_g * (data_simul$catb_1 == "1" & data_simul$catb_2 == "3") -
             catb13_g * (data_simul$catb_1 == "3" & data_simul$catb_2 == "1") +
             catb14_g * (data_simul$catb_1 == "1" & data_simul$catb_2 == "4") -
             catb14_g * (data_simul$catb_1 == "4" & data_simul$catb_2 == "1") +
             catb23_g * (data_simul$catb_1 == "2" & data_simul$catb_2 == "3") -
             catb23_g * (data_simul$catb_1 == "3" & data_simul$catb_2 == "2") +
             catb24_g * (data_simul$catb_1 == "2" & data_simul$catb_2 == "4") -
             catb24_g * (data_simul$catb_1 == "4" & data_simul$catb_2 == "2") +
             catb34_g * (data_simul$catb_1 == "3" & data_simul$catb_2 == "4") -
             catb34_g * (data_simul$catb_1 == "4" & data_simul$catb_2 == "3"))
  
  # Simulate choice
  data_simul$choice <- rbinom(nrow(data_simul), 1, data_simul$probability)
  
  # Reshape wide to long
  data_simul$taskno <- c(1:nrow(data_simul))
  data_simul_1 <- data_simul[, c("cont_1", "dich_1", "cata_1", "catb_1",
                                 "group", "choice", "taskno")]
  colnames(data_simul_1) <- c("cont", "dich", "cata", "catb",
                              "group", "choice", "taskno")
  data_simul_2 <- data_simul[, c("cont_2", "dich_2", "cata_2", "catb_2",
                                 "group", "choice", "taskno")]
  colnames(data_simul_2) <- c("cont", "dich", "cata", "catb",
                              "group", "choice", "taskno")
  data_simul_2$choice <- 1 - data_simul_2$choice
  data_simul_long <- rbind(data_simul_1, data_simul_2)
  levels(data_simul_long$catb) <- c("a", "b", "c", "d")
  
  
  ## Estimation
  
  # Prepare estimation objects
  object_for_estimation <- conjacp.prepdata(choice ~ cont + dich + cata * catb,
                                            data = data_simul_long,
                                            tasks = "taskno",
                                            subgroups = "group")
  
  # ACP
  acp_temp              <- conjacp.estimation(object_for_estimation,
                                              estimand = "acp",
                                              subset = list("group", "1"),
                                              adjust = FALSE)
  acp_temp_adjust       <- conjacp.estimation(object_for_estimation,
                                              estimand = "acp",
                                              subset = list("group", "1"),
                                              adjust = TRUE)
  
  # DACP
  dacp_temp             <- conjacp.estimation(object_for_estimation,
                                              estimand = "dacp",
                                              by = "group",
                                              adjust = FALSE)
  dacp_temp_adjust      <- conjacp.estimation(object_for_estimation,
                                              estimand = "dacp",
                                              by = "group",
                                              adjust = TRUE)
  
  # Direct pairwise preferences
  p_temp                <- conjacp.estimation(object_for_estimation,
                                              estimand = "p",
                                              subset = list("group", "1"),
                                              adjust = FALSE)
  p_temp_adjust         <- conjacp.estimation(object_for_estimation,
                                              estimand = "p",
                                              subset = list("group", "1"),
                                              adjust = TRUE)
  
  list(c(acp_temp$estimates, acp_temp$estimates_alt),
       c(sqrt(diag(acp_temp$vcov)), sqrt(diag(acp_temp$vcov_alt))),
       c(acp_temp_adjust$estimates, acp_temp_adjust$estimates_alt),
       c(sqrt(diag(acp_temp_adjust$vcov)), sqrt(diag(acp_temp_adjust$vcov_alt))),
       c(dacp_temp$estimates, dacp_temp$estimates_alt),
       c(sqrt(diag(dacp_temp$vcov)), sqrt(diag(dacp_temp$vcov_alt))),
       c(dacp_temp_adjust$estimates, dacp_temp_adjust$estimates_alt),
       c(sqrt(diag(dacp_temp_adjust$vcov)), sqrt(diag(dacp_temp_adjust$vcov_alt))),
       c(p_temp$estimates, p_temp$estimates_alt),
       c(sqrt(diag(p_temp$vcov)), sqrt(diag(p_temp$vcov_alt))),
       c(p_temp_adjust$estimates, p_temp_adjust$estimates_alt),
       c(sqrt(diag(p_temp_adjust$vcov)), sqrt(diag(p_temp_adjust$vcov_alt))))
  
  # End loop
}

# Save simulations results
simulation_results <- list(estimate_acp         = t(sapply(simulation_results, "[[", 1)),
                           se_acp               = t(sapply(simulation_results, "[[", 2)),
                           estimate_acp_adjust  = t(sapply(simulation_results, "[[", 3)),
                           se_acp_adjust        = t(sapply(simulation_results, "[[", 4)),
                           estimate_dacp        = t(sapply(simulation_results, "[[", 5)),
                           se_dacp              = t(sapply(simulation_results, "[[", 6)),
                           estimate_dacp_adjust = t(sapply(simulation_results, "[[", 7)),
                           se_dacp_adjust       = t(sapply(simulation_results, "[[", 8)),
                           estimate_p           = t(sapply(simulation_results, "[[", 9)),
                           se_p                 = t(sapply(simulation_results, "[[", 10)),
                           estimate_p_adjust    = t(sapply(simulation_results, "[[", 11)),
                           se_p_adjust          = t(sapply(simulation_results, "[[", 12)))
save(simulation_results, file = "Output/SimulationIntermediate.RData")




#### SIMULATIONS - WITH INTERACTION ####

### This chunk of code takes time to finish. To directly obtain the tables,
### skip to the TABLES section to load previously simulated data.

# Number of simulations
n_simul <- 5000

# Begin loop
simulation_results_int <- foreach(icount(n_simul)) %dopar% {
  
  
  ## Simulate data
  
  # Simulate attributes
  # The simulated sample size is four times the size of the expected sample size because
  # observations with forbidden combinations will be removed, and because we want two samples,
  # one for each subgroup.
  data_simul <- data.frame(cont_1 = round(runif(2 * n, 18, 65)),
                           cont_2 = round(runif(2 * n, 18, 65)),
                           dich_1 = sample(c(1,2), 2 * n, replace = TRUE),
                           dich_2 = sample(c(1,2), 2 * n, replace = TRUE),
                           cata_1 = sample(c(1:4), 2 * n, replace = TRUE),
                           cata_2 = sample(c(1:4), 2 * n, replace = TRUE),
                           catb_1 = sample(c(1:4), 2 * n, replace = TRUE),
                           catb_2 = sample(c(1:4), 2 * n, replace = TRUE))
  
  # Remove forbidden combinations
  data_simul <- data_simul[!((data_simul$cata_1 == 4 &
                                data_simul$catb_1 %in% c(3,4)) |
                               (data_simul$cata_2 == 4 &
                                  data_simul$catb_2 %in% c(3,4))),]
  
  # Keep 2*n observations
  data_simul <- data_simul[c(1:n),]
  
  # Convert attributes into factor variables
  data_simul[,-c(1,2)] <- lapply(data_simul[,-c(1,2)], as.factor)
  
  # Calculate contrasting selection probability
  data_simul$probability <-
    .5 + cont * (data_simul$cont_1 - data_simul$cont_2) +
    dich12 * (data_simul$dich_1 == "1" & data_simul$dich_2 == "2") -
    dich12 * (data_simul$dich_1 == "2" & data_simul$dich_2 == "1") +
    cata12 * (data_simul$cata_1 == "1" & data_simul$cata_2 == "2") -
    cata12 * (data_simul$cata_1 == "2" & data_simul$cata_2 == "1") +
    cata13 * (data_simul$cata_1 == "1" & data_simul$cata_2 == "3") -
    cata13 * (data_simul$cata_1 == "3" & data_simul$cata_2 == "1") +
    cata14 * (data_simul$cata_1 == "1" & data_simul$cata_2 == "4") -
    cata14 * (data_simul$cata_1 == "4" & data_simul$cata_2 == "1") +
    cata23 * (data_simul$cata_1 == "2" & data_simul$cata_2 == "3") -
    cata23 * (data_simul$cata_1 == "3" & data_simul$cata_2 == "2") +
    cata24 * (data_simul$cata_1 == "2" & data_simul$cata_2 == "4") -
    cata24 * (data_simul$cata_1 == "4" & data_simul$cata_2 == "2") +
    cata34 * (data_simul$cata_1 == "3" & data_simul$cata_2 == "4") -
    cata34 * (data_simul$cata_1 == "4" & data_simul$cata_2 == "3") +
    catb12_123 * (data_simul$catb_1 == "1" & data_simul$catb_2 == "2" &
                    data_simul$cata_1 %in% c("1", "2", "3") & data_simul$cata_2 %in% c("1", "2", "3")) -
    catb12_123 * (data_simul$catb_1 == "2" & data_simul$catb_2 == "1" &
                    data_simul$cata_1 %in% c("1", "2", "3") & data_simul$cata_2 %in% c("1", "2", "3")) +
    catb12_4 * (data_simul$catb_1 == "1" & data_simul$catb_2 == "2" &
                  data_simul$cata_1 == "4" & data_simul$cata_2 == "4") -
    catb12_4 * (data_simul$catb_1 == "2" & data_simul$catb_2 == "1" &
                  data_simul$cata_1 == "4" & data_simul$cata_2 == "4") +
    catb12_1234 * (data_simul$catb_1 == "1" & data_simul$catb_2 == "2" &
                     data_simul$cata_1 %in% c("1", "2", "3") & data_simul$cata_2 == "4") -
    catb12_1234 * (data_simul$catb_1 == "2" & data_simul$catb_2 == "1" &
                     data_simul$cata_1 == "4" & data_simul$cata_2 %in% c("1", "2", "3")) +
    catb12_4123 * (data_simul$catb_1 == "1" & data_simul$catb_2 == "2" &
                     data_simul$cata_1 == "4" & data_simul$cata_2 %in% c("1", "2", "3")) -
    catb12_4123 * (data_simul$catb_1 == "2" & data_simul$catb_2 == "1" &
                     data_simul$cata_1 %in% c("1", "2", "3") & data_simul$cata_2 == "4") +
    catb13 * (data_simul$catb_1 == "1" & data_simul$catb_2 == "3") -
    catb13 * (data_simul$catb_1 == "3" & data_simul$catb_2 == "1") +
    catb14 * (data_simul$catb_1 == "1" & data_simul$catb_2 == "4") -
    catb14 * (data_simul$catb_1 == "4" & data_simul$catb_2 == "1") +
    catb23 * (data_simul$catb_1 == "2" & data_simul$catb_2 == "3") -
    catb23 * (data_simul$catb_1 == "3" & data_simul$catb_2 == "2") +
    catb24 * (data_simul$catb_1 == "2" & data_simul$catb_2 == "4") -
    catb24 * (data_simul$catb_1 == "4" & data_simul$catb_2 == "2") +
    catb34 * (data_simul$catb_1 == "3" & data_simul$catb_2 == "4") -
    catb34 * (data_simul$catb_1 == "4" & data_simul$catb_2 == "3")
  
  # Simulate choice
  data_simul$choice <- rbinom(nrow(data_simul), 1, data_simul$probability)
  
  # Reshape wide to long
  data_simul$taskno <- c(1:nrow(data_simul))
  data_simul_1 <- data_simul[, c("cont_1", "dich_1", "cata_1", "catb_1",
                                 "choice", "taskno")]
  colnames(data_simul_1) <- c("cont", "dich", "cata", "catb",
                              "choice", "taskno")
  data_simul_2 <- data_simul[, c("cont_2", "dich_2", "cata_2", "catb_2",
                                 "choice", "taskno")]
  colnames(data_simul_2) <- c("cont", "dich", "cata", "catb",
                              "choice", "taskno")
  data_simul_2$choice <- 1 - data_simul_2$choice
  data_simul_long <- rbind(data_simul_1, data_simul_2)
  levels(data_simul_long$catb) <- c("a", "b", "c", "d")
  
  
  ## Estimation
  
  # Prepare estimation objects
  object_for_estimation <- conjacp.prepdata(choice ~ cont + dich + cata * catb,
                                            data = data_simul_long,
                                            tasks = "taskno")
  
  # ACP
  acp_temp              <- conjacp.estimation(object_for_estimation,
                                              estimand = "acp",
                                              adjust = FALSE)
  acp_temp_adjust       <- conjacp.estimation(object_for_estimation,
                                              estimand = "acp",
                                              adjust = TRUE)
  
  list(c(acp_temp$estimates, acp_temp$estimates_alt),
       c(sqrt(diag(acp_temp$vcov)), sqrt(diag(acp_temp$vcov_alt))),
       c(acp_temp_adjust$estimates, acp_temp_adjust$estimates_alt),
       c(sqrt(diag(acp_temp_adjust$vcov)), sqrt(diag(acp_temp_adjust$vcov_alt))))
  
  # End loop
}

# Save simulations results
simulation_results_int <- list(estimate_acp         = t(sapply(simulation_results_int, "[[", 1)),
                               se_acp               = t(sapply(simulation_results_int, "[[", 2)),
                               estimate_acp_adjust  = t(sapply(simulation_results_int, "[[", 3)),
                               se_acp_adjust        = t(sapply(simulation_results_int, "[[", 4)))
save(simulation_results_int, file = "Output/SimulationIntermediate_int.RData")



#### TABLES ####

# Load simulated data
load("Output/SimulationIntermediate.RData")
load("Output/SimulationIntermediate_int.RData")


## ACP

# Target values
target_acp <- c(cont,
                dich12, -dich12,
                1/3 * (cata12 + cata13 + cata14),
                1/3 * (cata12 + cata13 + cata14),
                1/3 * (- cata12 + cata23 + cata24),
                1/3 * (- cata12 + cata23 + cata24),
                1/3 * (- cata13 - cata23 + cata34),
                1/3 * (- cata13 - cata23 + cata34),
                1/3 * (- cata14 - cata24 - cata34),
                1/3 * (catb12 + catb13 + catb14),
                1/3 * (catb12 + catb13 + catb14),
                1/3 * (- catb12 + catb23 + catb24),
                1/3 * (- catb12 + catb23 + catb24),
                1/3 * (- catb13 - catb23 + catb34),
                1/3 * (- catb14 - catb24 - catb34))
names(target_acp) <- c("cont", "dich.1", "dich.2",
                       "cata.1..c1", "cata.1..c2", "cata.2..c1", "cata.2..c2",
                       "cata.3..c1", "cata.3..c2", "cata.4..c1",
                       "catb.a..c1", "catb.a..c2", "catb.b..c1", "catb.b..c2",
                       "catb.c..c1", "catb.d..c1")

# Create empty table
nrow_table <- length(target_acp)
table_acp_unadj <- data.frame(estimate = rep(NA, nrow_table),
                              target   = rep(NA, nrow_table),
                              mean     = rep(NA, nrow_table),
                              sd       = rep(NA, nrow_table),
                              bias     = rep(NA, nrow_table),
                              mae      = rep(NA, nrow_table),
                              mean.se  = rep(NA, nrow_table),
                              cov      = rep(NA, nrow_table))
table_acp_adj <- data.frame(estimate = rep(NA, nrow_table),
                            target   = rep(NA, nrow_table),
                            mean     = rep(NA, nrow_table),
                            sd       = rep(NA, nrow_table),
                            bias     = rep(NA, nrow_table),
                            mae      = rep(NA, nrow_table),
                            mean.se  = rep(NA, nrow_table),
                            cov      = rep(NA, nrow_table))

# Fill tablee
estimate_acp        <- simulation_results[["estimate_acp"]][,c(1:4, 15, 5, 16, 6, 17, 7, 8, 19, 9, 20, 10, 11)]
se_acp              <- simulation_results[["se_acp"]][,c(1:4, 15, 5, 16, 6, 17, 7, 8, 19, 9, 20, 10, 11)]
estimate_acp_adjust <- simulation_results[["estimate_acp_adjust"]][,c(1:4, 15, 5, 16, 6, 17, 7, 8, 19, 9, 20, 10, 11)]
se_acp_adjust       <- simulation_results[["se_acp_adjust"]][,c(1:4, 15, 5, 16, 6, 17, 7, 8, 19, 9, 20, 10, 11)]
for (var in 1:nrow_table) {
  row                       <- var
  table_acp_unadj$target[row] <- target_acp[var]
  table_acp_adj$target[row] <- target_acp[var]
  table_acp_unadj$mean[row] <- mean(estimate_acp[,var])
  table_acp_adj$mean[row] <- mean(estimate_acp_adjust[,var])
  table_acp_unadj$mean.se[row] <- mean(se_acp[,var])
  table_acp_adj$mean.se[row] <- mean(se_acp_adjust[,var])
  table_acp_unadj$sd[row] <- sd(estimate_acp[,var])
  table_acp_adj$sd[row] <- sd(estimate_acp_adjust[,var])
  table_acp_unadj$bias[row] <- mean(estimate_acp[,var]) - target_acp[var]
  table_acp_adj$bias[row] <- mean(estimate_acp_adjust[,var]) - target_acp[var]
  mae_unadj <- abs(estimate_acp[,var] - target_acp[var])
  mae_adj <- abs(estimate_acp_adjust[,var] - target_acp[var])
  table_acp_unadj$mae[row] <- mean(mae_unadj)
  table_acp_adj$mae[row] <- mean(mae_adj)
  cov_unadj <- ifelse(estimate_acp[,var] - 1.96 * se_acp[,var] <= target_acp[var] &
                  estimate_acp[,var] + 1.96 * se_acp[,var] >= target_acp[var], 1, 0)
  cov_adj <- ifelse(estimate_acp_adjust[,var] - 1.96 * se_acp_adjust[,var] <= target_acp[var] &
                      estimate_acp_adjust[,var] + 1.96 * se_acp_adjust[,var] >= target_acp[var], 1, 0)
  table_acp_unadj$cov[row] <- mean(cov_unadj)
  table_acp_adj$cov[row] <- mean(cov_adj)
}
table_acp_unadj[,2:8] <- apply(table_acp_unadj[,2:8], c(1, 2), round, 3)
table_acp_adj[,2:8] <- apply(table_acp_adj[,2:8], c(1, 2), round, 3)


## ACP - w/ interactions

# Target values
target_acp_int <- c(1/3 * (catb12_123 + catb13 + catb14),
                    1/3 * ((9/16 * catb12_123 + 3/16 * catb12_4123 + 3/16 * catb12_1234 + 1/16 * catb12_4) +
                             catb13 + catb14),
                    1/3 * (- catb12_123 + catb23 + catb24),
                    1/3 * (- (9/16 * catb12_123 + 3/16 * catb12_4123 + 3/16 * catb12_1234 + 1/16 * catb12_4) +
                             catb23 + catb24),
                    1/3 * (- catb13 - catb23 + catb34),
                    1/3 * (- catb14 - catb24 - catb34))
names(target_acp_int) <- c("catb.a..c1", "catb.a..c2", "catb.b..c1", "catb.b..c2", "catb.c..c1", "catb.d..c1")


# Create empty table
nrow_table <- length(target_acp_int)
table_acp_int_unadj <- data.frame(estimate = rep(NA, nrow_table),
                                  target   = rep(NA, nrow_table),
                                  mean     = rep(NA, nrow_table),
                                  sd       = rep(NA, nrow_table),
                                  bias     = rep(NA, nrow_table),
                                  mae      = rep(NA, nrow_table),
                                  mean.se  = rep(NA, nrow_table),
                                  cov      = rep(NA, nrow_table))
table_acp_int_adj <- data.frame(estimate = rep(NA, nrow_table),
                                target   = rep(NA, nrow_table),
                                mean     = rep(NA, nrow_table),
                                sd       = rep(NA, nrow_table),
                                bias     = rep(NA, nrow_table),
                                mae      = rep(NA, nrow_table),
                                mean.se  = rep(NA, nrow_table),
                                cov      = rep(NA, nrow_table))

# Fill table
estimate_acp_int        <- simulation_results_int[["estimate_acp"]][,c(8, 19, 9, 20, 10, 11)]
se_acp_int              <- simulation_results_int[["se_acp"]][,c(8, 19, 9, 20, 10, 11)]
estimate_acp_adjust_int <- simulation_results_int[["estimate_acp_adjust"]][,c(8, 19, 9, 20, 10, 11)]
se_acp_adjust_int       <- simulation_results_int[["se_acp_adjust"]][,c(8, 19, 9, 20, 10, 11)]
for (var in 1:nrow_table) {
  row <- var
  table_acp_int_unadj$target[row] <- target_acp_int[var]
  table_acp_int_adj$target[row] <- target_acp_int[var]
  table_acp_int_unadj$mean[row] <- mean(estimate_acp_int[,var])
  table_acp_int_adj$mean[row] <- mean(estimate_acp_adjust_int[,var])
  table_acp_int_unadj$mean.se[row] <- mean(se_acp_int[,var])
  table_acp_int_adj$mean.se[row] <- mean(se_acp_adjust_int[,var])
  table_acp_int_unadj$sd[row] <- sd(estimate_acp_int[,var])
  table_acp_int_adj$sd[row] <- sd(estimate_acp_adjust_int[,var])
  table_acp_int_unadj$bias[row] <- mean(estimate_acp_int[,var]) - target_acp_int[var]
  table_acp_int_adj$bias[row] <- mean(estimate_acp_adjust_int[,var]) - target_acp_int[var]
  mae_unadj <- abs(estimate_acp_int[,var] - target_acp_int[var])
  mae_adj <- abs(estimate_acp_adjust_int[,var] - target_acp_int[var])
  table_acp_int_unadj$mae[row] <- mean(mae_unadj)
  table_acp_int_adj$mae[row] <- mean(mae_adj)
  cov_unadj <- ifelse(estimate_acp_int[,var] - 1.96 * se_acp_int[,var] <= target_acp_int[var] &
                        estimate_acp_int[,var] + 1.96 * se_acp_int[,var] >= target_acp_int[var], 1, 0)
  cov_adj <- ifelse(estimate_acp_adjust_int[,var] - 1.96 * se_acp_adjust_int[,var] <= target_acp_int[var] &
                      estimate_acp_adjust_int[,var] + 1.96 * se_acp_adjust_int[,var] >= target_acp_int[var], 1, 0)
  table_acp_int_unadj$cov[row] <- mean(cov_unadj)
  table_acp_int_adj$cov[row] <- mean(cov_adj)
}
table_acp_int_unadj[,2:8] <- apply(table_acp_int_unadj[,2:8], c(1, 2), round, 3)
table_acp_int_adj[,2:8] <- apply(table_acp_int_adj[,2:8], c(1, 2), round, 3)

  # Create finale table for paper
  table_acp_final <- rbind(table_acp_unadj, table_acp_int_unadj)
  table_acp_final$estimate <- c("Continuous: Slope", "Binary: Level 1", "Binary: Level 2",
                                "Cat1: Level 1 (conditional)", "Cat1: Level 1 (comparable pairs)",
                                "Cat1: Level 2 (conditional)", "Cat1: Level 2 (comparable pairs)",
                                "Cat1: Level 3 (conditional)", "Cat1: Level 3 (comparable pairs)",
                                "Cat1: Level 4",
                                "Cat2: Level 1 (conditional)", "Cat2: Level 1 (comparable pairs)",
                                "Cat2: Level 2 (conditional)", "Cat2: Level 2 (comparable pairs)",
                                "Cat2: Level 3", "Cat2: Level 4",
                                "Cat2_int: Level 1 (conditional)", "Cat2_int: Level 1 (comparable pairs)",
                                "Cat2_int: Level 2 (conditional)", "Cat2_int: Level 2 (comparable pairs)",
                                "Cat2_int: Level 3", "Cat2_int: Level 4")
  write_csv(table_acp_final, "Output/simulations_acp.csv")



## DACP

# Target values
target_dacp <- c(cont - cont_g,
                 dich12 - dich12_g, -dich12 + dich12_g,
                 1/3 * ((cata12 + cata13 + cata14) - (cata12_g + cata13_g + cata14_g)),
                 1/3 * ((- cata12 + cata23 + cata24) - (- cata12_g + cata23_g + cata24_g)),
                 1/3 * ((- cata13 - cata23 + cata34) - (- cata13_g - cata23_g + cata34_g)),
                 1/3 * ((- cata14 - cata24 - cata34) - (- cata14_g - cata24_g - cata34_g)),
                 1/3 * ((catb12 + catb13 + catb14) - (catb12_g + catb13_g + catb14_g)),
                 1/3 * ((- catb12 + catb23 + catb24) - (- catb12_g + catb23_g + catb24_g)),
                 1/3 * ((- catb13 - catb23 + catb34) - (- catb13_g - catb23_g + catb34_g)),
                 1/3 * ((- catb14 - catb24 - catb34) - (- catb14_g - catb24_g - catb34_g)))
names(target_dacp) <- c("cont", "dich.1", "dich.2",
                        "cata.1..c1", "cata.2..c1", "cata.3..c1", "cata.4..c1",
                        "catb.a..c1", "catb.b..c1", "catb.c..c1", "catb.d..c1")

# Create mpty table
nrow_table <- length(target_dacp)
table_dacp_unadj <- data.frame(estimate = rep(NA, nrow_table),
                               target   = rep(NA, nrow_table),
                               mean     = rep(NA, nrow_table),
                               sd       = rep(NA, nrow_table),
                               bias     = rep(NA, nrow_table),
                               mae      = rep(NA, nrow_table),
                               mean.se  = rep(NA, nrow_table),
                               cov      = rep(NA, nrow_table))
table_dacp_adj <- data.frame(estimate = rep(NA, nrow_table),
                             target   = rep(NA, nrow_table),
                             mean     = rep(NA, nrow_table),
                             sd       = rep(NA, nrow_table),
                             bias     = rep(NA, nrow_table),
                             mae      = rep(NA, nrow_table),
                             mean.se  = rep(NA, nrow_table),
                             cov      = rep(NA, nrow_table))

# Fill table
estimate_dacp        <- simulation_results[["estimate_dacp"]][, c(1:11)]
se_dacp              <- simulation_results[["se_dacp"]][, c(1:11)]
estimate_dacp_adjust <- simulation_results[["estimate_dacp_adjust"]][, c(1:11)]
se_dacp_adjust       <- simulation_results[["se_dacp_adjust"]][, c(1:11)]
for (var in 1:nrow_table) {
  row <- var
  table_dacp_unadj$target[row] <- target_dacp[var]
  table_dacp_adj$target[row] <- target_dacp[var]
  table_dacp_unadj$mean[row] <- mean(estimate_dacp[,var])
  table_dacp_adj$mean[row] <- mean(estimate_dacp_adjust[,var])
  table_dacp_unadj$mean.se[row] <- mean(se_dacp[,var])
  table_dacp_adj$mean.se[row] <- mean(se_dacp_adjust[,var])
  table_dacp_unadj$sd[row] <- sd(estimate_dacp[,var])
  table_dacp_adj$sd[row] <- sd(estimate_dacp_adjust[,var])
  table_dacp_unadj$bias[row] <- mean(estimate_dacp[,var]) - target_dacp[var]
  table_dacp_adj$bias[row] <- mean(estimate_dacp_adjust[,var]) - target_dacp[var]
  mae_unadj <- abs(estimate_dacp[,var] - target_dacp[var])
  mae_adj <- abs(estimate_dacp_adjust[,var] - target_dacp[var])
  table_dacp_unadj$mae[row] <- mean(mae_unadj)
  table_dacp_adj$mae[row] <- mean(mae_adj)
  cov_unadj <- ifelse(estimate_dacp[,var] - 1.96 * se_dacp[,var] <= target_dacp[var] &
                        estimate_dacp[,var] + 1.96 * se_dacp[,var] >= target_dacp[var], 1, 0)
  cov_adj <- ifelse(estimate_dacp_adjust[,var] - 1.96 * se_dacp_adjust[,var] <= target_dacp[var] &
                      estimate_dacp_adjust[,var] + 1.96 * se_dacp_adjust[,var] >= target_dacp[var], 1, 0)
  table_dacp_unadj$cov[row] <- mean(cov_unadj)
  table_dacp_adj$cov[row] <- mean(cov_adj)
}
table_dacp_unadj[,2:8] <- apply(table_dacp_unadj[,2:8], c(1, 2), round, 3)
table_dacp_adj[,2:8] <- apply(table_dacp_adj[,2:8], c(1, 2), round, 3)

  # Create finale table for paper
  table_dacp_final <- table_dacp_unadj
  table_dacp_final$estimate <- c("Continuous: Slope", "Binary: Level 1", "Binary: Level 2",
                                "Cat1: Level 1", "Cat1: Level 2", "Cat1: Level 3", "Cat1: Level 4",
                                "Cat2: Level 1", "Cat2: Level 2", "Cat2: Level 3", "Cat2: Level 4")
  write_csv(table_dacp_final, "Output/simulations_dacp.csv")



## P

# Target values
target_p <- c(cont,
              dich12,
              cata12, cata13, cata14, cata23, cata24, cata34,
              catb12, catb13, catb14, catb23, catb24, catb34)

# Create mpty table
nrow_table <- length(target_p)
table_p_unadj <- data.frame(estimate = rep(NA, nrow_table),
                            target   = rep(NA, nrow_table),
                            mean     = rep(NA, nrow_table),
                            sd       = rep(NA, nrow_table),
                            bias     = rep(NA, nrow_table),
                            mae      = rep(NA, nrow_table),
                            mean.se  = rep(NA, nrow_table),
                            cov      = rep(NA, nrow_table))
table_p_adj <- data.frame(estimate = rep(NA, nrow_table),
                          target   = rep(NA, nrow_table),
                          mean     = rep(NA, nrow_table),
                          sd       = rep(NA, nrow_table),
                          bias     = rep(NA, nrow_table),
                          mae      = rep(NA, nrow_table),
                          mean.se  = rep(NA, nrow_table),
                          cov      = rep(NA, nrow_table))

# Fill table
estimate_p           <- simulation_results[["estimate_p"]][,c(1:14)]
se_p                 <- simulation_results[["se_p"]][,c(1:14)]
estimate_p_adjust    <- simulation_results[["estimate_p_adjust"]][,c(1:14)]
se_p_adjust          <- simulation_results[["se_p_adjust"]][,c(1:14)]
for (var in 1:nrow_table) {
  row <- var
  table_p_unadj$target[row] <- target_p[var]
  table_p_adj$target[row] <- target_p[var]
  table_p_unadj$mean[row] <- mean(estimate_p[,var])
  table_p_adj$mean[row] <- mean(estimate_p_adjust[,var])
  table_p_unadj$mean.se[row] <- mean(se_p[,var])
  table_p_adj$mean.se[row] <- mean(se_p_adjust[,var])
  table_p_unadj$sd[row] <- sd(estimate_p[,var])
  table_p_adj$sd[row] <- sd(estimate_p_adjust[,var])
  table_p_unadj$bias[row] <- mean(estimate_p[,var]) - target_p[var]
  table_p_adj$bias[row] <- mean(estimate_p_adjust[,var]) - target_p[var]
  mae_unadj <- abs(estimate_p[,var] - target_p[var])
  mae_adj <- abs(estimate_p_adjust[,var] - target_p[var])
  table_p_unadj$mae[row] <- mean(mae_unadj)
  table_p_adj$mae[row] <- mean(mae_adj)
  cov_unadj <- ifelse(estimate_p[,var] - 1.96 * se_p[,var] <= target_p[var] &
                        estimate_p[,var] + 1.96 * se_p[,var] >= target_p[var], 1, 0)
  cov_adj <- ifelse(estimate_p_adjust[,var] - 1.96 * se_p_adjust[,var] <= target_p[var] &
                      estimate_p_adjust[,var] + 1.96 * se_p_adjust[,var] >= target_p[var], 1, 0)
  table_p_unadj$cov[row] <- mean(cov_unadj)
  table_p_adj$cov[row] <- mean(cov_adj)
}
table_p_unadj[,2:8] <- apply(table_p_unadj[,2:8], c(1, 2), round, 3)
table_p_adj[,2:8] <- apply(table_p_adj[,2:8], c(1, 2), round, 3)

  # Create finale table for paper
  table_p_final <- table_p_unadj
  table_p_final$estimate <- c("Continuous: Slope", "Binary: Levels 1-2", "Cat1: Levels 1-2",
                              "Cat1: Levels 1-3", "Cat1: Levels 1-4", "Cat1: Levels 2-3",
                              "Cat1: Levels 2-4", "Cat1: Levels 3-4", "Cat2: Levels 1-2",
                              "Cat2: Levels 1-3", "Cat2: Levels 1-4", "Cat2: Levels 2-3",
                              "Cat2: Levels 2-4", "Cat2: Levels 3-4")
  write_csv(table_p_final, "Output/simulations_p.csv")
  