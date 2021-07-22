# Replicate the analyses of the immigrant experiment (Hainmueller, Hopkins and Yamamoto 2014, and
# Hanmueller and Hopkins 2015) using the ACP framework (Ganter 2021)
# Author: Flavien Ganter
# Created on December 29, 2019; last modified on July 16, 2021



#### PRELIMINARIES ####

# Clear working space
rm(list = ls())

# Set working directory
setwd("") ### specify path to root replication folder

# Packages and functions
library(tidyverse)
library(haven)
library(sjlabelled)
library(xlsx)
library(patchwork)
source("Functions/conjacp.R") ### Function that calculates ACPs
source("Functions/FunctionVCOVCluster.R") ### Function that calculates clustered SEs
source("Functions/FunctionTableGraph.R") ### Function that prepare the data frame for result graphs
source("Functions/theme_graph.R")

# Graph theme add-on
extrafont::loadfonts()
cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00",
               "#CC79A7", "#999999", "#56B4E9", "#F0E442")



#### DATA ####


# Import data
data_long <- read_stata("Data/HainmuellerHopkins.dta")

# Convert into factors and get labels
data_long[, c(3:12)] <- lapply(data_long[, c(3:12)],
                               function(x) factor(x, labels = get_labels(x)))

# Recode outcome variable
data_long$choice <- ifelse(data_long$Chosen_Immigrant == "Yes", 1, 0)

# Create task variable
data_long$task <- cumsum(!duplicated(data_long[, c("contest_no","CaseID")]))

# Change reference levels
data_long$FeatExp     <- relevel(data_long$FeatExp, "One or two years of job training and experience")
data_long$FeatPlans   <- relevel(data_long$FeatPlans, "Will look for work after arriving in the U.S.")
data_long$FeatLang    <- relevel(data_long$FeatLang, "During admission interview, this applicant spoke broken English"  )
data_long$FeatEd      <- relevel(data_long$FeatEd, "Equivalent to completing high school in the US")
data_long$FeatJob     <- relevel(data_long$FeatJob, "Teacher")
data_long$FeatCountry <- relevel(data_long$FeatCountry, "India")

# Create CONJACP object for ACP and DACP estimation
conjacp_data <- conjacp.prepdata(choice ~ FeatEd * FeatJob + FeatGender + FeatCountry * FeatReason +
                                   FeatExp + FeatPlans + FeatTrips + FeatLang,
                                 data = data_long,
                                 tasks = "task",
                                 id = "CaseID")



#### ACP & AMCE ####


## Estimate quantities of interest

# ACPs
results_acp <- conjacp.estimation(conjacp_data,
                                  estimand = "acp",
                                  adjust = FALSE)
  
# AMCEs
  
  # Completely independently randomized attributes
  
    # Create empty vectors to store results
    ind_estimate <- NULL
    ind_se       <- NULL
    
    # Loop over independently randomized attributes
    for (attribute in c("FeatGender", "FeatExp", "FeatPlans", "FeatTrips", "FeatLang")) {
      
      # create data frame for model
      data_model <- data_long[, c(attribute, "choice")]
      names(data_model)[-ncol(data_model)] <- paste0(names(data_model)[-ncol(data_model)], ".")
      
      # estimate model
      model <- lm(choice ~ ., data = data_model)
      
      # store estimates
      ind_estimate <- c(ind_estimate, coef(model)[-1])
      vcov         <- vcovCluster(model, data_long$CaseID)
      ind_se       <- c(ind_se, sqrt(diag(vcov))
                        [2:(nlevels(as.data.frame(data_long)[, match(attribute, colnames(data_long))]))])
      
    }

  # Conditionally independently randomized attributes
    
    # Country * Reason
    
      # Estimate model
      model <- lm(choice ~ FeatCountry * FeatReason, data = data_long)
      coef  <- coef(model)[!is.na(coef(model))]
      vcov  <- vcovCluster(model, data_long$CaseID)
      
      # Weights
      weight_reason <- matrix(c(1/10 * c(rep(0, 10), 10, 0, rep(1, 9), rep(0, 3)),
                                1/4 * c(rep(0, 11), 4, rep(0, 9), rep(1, 3))),
                              ncol = 2, nrow = 24)
      weight_country <- matrix(NA, ncol = 9, nrow = 24)
      for (k in 2:10) {
        weight_country[, k-1] <- 1/2 * c(rep(0, k-1), 2, rep(0, 10), 1, rep(0, 13-k))
      }
      
      # AMCEs
      cond_estimate <- c(t(weight_reason) %*% as.matrix(coef),
                         t(weight_country) %*% as.matrix(coef))
      cond_se       <- c(sqrt(diag(t(weight_reason) %*% vcov %*% weight_reason)),
                         sqrt(diag(t(weight_country) %*% vcov %*% weight_country)))
      
    # Education * Occupation
      
      # Estimate model
      model <- lm(choice ~ FeatEd * FeatJob, data = data_long)
      coef  <- coef(model)[!is.na(coef(model))]
      vcov  <- vcovCluster(model, data_long$CaseID)
      
      # Weights
      weight_educ <- matrix(NA, ncol = 6, nrow = 61)
      for (k in 2:7) {
        weight_educ[, k-1] <- 1/7 * c(rep(0, k-1), 7, rep(0, 15), 1, rep(0, 5), 1,
                                      rep(0, 5), 1, rep(0, 7), 1, rep(0, 5), 1,
                                      rep(0, 7), 1, rep(0, 11-k))
      }
      weight_occ <- matrix(c(1/7 * c(rep(0, 7), 7, rep(0, 9), rep(1, 6), rep(0, 38)),
                             1/7 * c(rep(0, 8), 7, rep(0, 14), rep(1, 6), rep(0, 32)),
                             1/7 * c(rep(0, 9), 7, rep(0, 19), rep(1, 6), rep(0, 26)),
                             1/3 * c(rep(0, 10), 3, rep(0, 24), rep(1, 2), rep(0, 24)),
                             1/7 * c(rep(0, 11), 7, rep(0, 25), rep(1, 6), rep(0, 18)),
                             1/7 * c(rep(0, 12), 7, rep(0, 30), rep(1, 6), rep(0, 12)),
                             1/3 * c(rep(0, 13), 3, rep(0, 35), rep(1, 2), rep(0, 10)),
                             1/7 * c(rep(0, 14), 7, rep(0, 36), rep(1, 6), rep(0, 4)),
                             1/3 * c(rep(0, 15), 3, rep(0, 41), rep(1, 2), rep(0, 2)),
                             1/3 * c(rep(0, 16), 3, rep(0, 42), rep(1, 2))),
                             ncol = 10, nrow = 61)
      
      # AMCEs
      cond_estimate <- c(cond_estimate,
                         t(weight_educ) %*% as.matrix(coef),
                         t(weight_occ) %*% as.matrix(coef))
      cond_se       <- c(cond_se,
                         sqrt(diag(t(weight_educ) %*% vcov %*% weight_educ)),
                         sqrt(diag(t(weight_occ) %*% vcov %*% weight_occ)))
      
    # Names
    cond_names <- NULL
    for (i in 2:nlevels(data_long$FeatReason)) {
      cond_names <- c(cond_names,
                      paste0("FeatReason.", levels(data_long$FeatReason)[i]))
    }
    for (i in 2:nlevels(data_long$FeatCountry)) {
      cond_names <- c(cond_names,
                      paste0("FeatCountry.", levels(data_long$FeatCountry)[i]))
    }
    for (i in 2:nlevels(data_long$FeatEd)) {
      cond_names <- c(cond_names,
                      paste0("FeatEd.", levels(data_long$FeatEd)[i]))
    }
    for (i in 2:nlevels(data_long$FeatJob)) {
      cond_names <- c(cond_names,
                      paste0("FeatJob.", levels(data_long$FeatJob)[i]))
    }

    
## Graph ACPs vs. AMCEs
    
# Prepare estimate table

  # ACPs
    
    # Unrestricted levels
    table_acp_cacpcond <- table.graph(results_acp,
                                      estimand = "acp",
                                      cacp = "conditional")
    table_acp_cacpcond$estimand        <- NA
    table_acp_cacpcond$estimand[1:29]  <- "ACP"
    table_acp_cacpcond$estimand[30:67] <- "CACP, Unrestricted levels"
  
    # Comparable pairs
    table_acp_cacpcomp <- table.graph(results_acp,
                                      estimand = "acp",
                                      cacp = "all_comparable")
    table_acp_cacpcomp$estimand        <- NA
    table_acp_cacpcomp$estimand[1:29]  <- "ACP"
    table_acp_cacpcomp$estimand[30:67] <- "CACP, Comparable pairs"
  
  table_acp <- rbind(table_acp_cacpcond, table_acp_cacpcomp)
  table_acp <- table_acp[order(table_acp$order),]
  table_acp <- table_acp[!duplicated(table_acp[, c("modality", "estimate")]),]
  table_acp <- table_acp[-c(32,34,36,38,56,60,66,68,80,82,84,86,88,90,98),]
  table_acp$type <- "(C)ACP"

  # AMCEs
  estimate            <- c(ind_estimate, cond_estimate)
  names(estimate)     <- c(names(estimate)[1:14], cond_names)
  results_amce        <- list(estimates = estimate,
                              vcov = NULL,
                              se = c(ind_se, cond_se))
  table_amce          <- table.graph(results_amce, estimand = "amce")
  table_amce$estimand <- "AMCE"
  table_amce$type     <- "AMCE"

table_acpamce <- rbind(table_acp, table_amce)
table_acpamce[, c("estimate", "se")] <- apply(table_acpamce[, c("estimate", "se")], 2, as.numeric)
hline <- data.frame(type = c("AMCE", "(C)ACP"), yint = c(0, 5))

# Plot graphs
immplot <- ggplot(table_acpamce[c(5:16,42:60),],
                        aes(y = estimate, x = modality, group = estimand)) +
  coord_flip(ylim = c(-.22, .2)) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se,
                      color = estimand, shape = estimand, fill = estimand),
                  position = position_dodge(width = .3), size = .2) +
  labs(y = "", x = "") +
  theme_fg(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        axis.text.y = element_text(hjust = 0 , vjust = .5 ),
        legend.position = "bottom",
        legend.box.margin = margin(-20, 0, 0, 0))  +
  scale_shape_manual(values = c(21, 22, 23, 23), name = "") +
  scale_fill_manual(values = cbPalette[c(1,3,4)], name = "") +
  scale_colour_manual(values = cbPalette[c(1,3,4)], name = "")
ggsave(immplot, filename = "Output/acpamce_imm.pdf",
       height = 5, width = 6, device = cairo_pdf)
  


#### RELATIVE IMPORTANCE ####
    
  
## Estimation

# Conditional: education
results_condEd <- conjacp.estimation(conjacp_data,
                                     estimand = "acp",
                                     condition = list("FeatEd",
                                                      c("Equivalent to completing a college degree in the US",
                                                        "Equivalent to completing a graduate degree in the US",
                                                        "Equivalent to completing two years of college in the US")),
                                     adjust = FALSE)

# Conditional: job
results_condJob <- conjacp.estimation(conjacp_data,
                                      estimand = "acp",
                                      condition = list("FeatJob",
                                                       c("Teacher", "Janitor", "Waiter", "Child care provider", "Gardener", 
                                                         "Construction worker", "Nurse")),
                                      adjust = FALSE)

# Conditional: country
results_condCountry <- conjacp.estimation(conjacp_data,
                                          estimand = "acp",
                                          condition = list("FeatCountry",
                                                           c("China", "Sudan", "Somalia", "Iraq")),
                                          adjust = FALSE)

# Conditional: reason
results_condReason <- conjacp.estimation(conjacp_data,
                                         estimand = "acp",
                                         condition = list("FeatReason",
                                                          c("Reunite with family members already in the U.S.",
                                                            "Seek better job in U.S.")),
                                         adjust = FALSE)


## Graph relative importance of attributes

# Prepare estimate table

  # Function: prepare table
  var.table <- function(object.var, k = 2) {
    tab1 <- data.frame(attribute = names(object.var$range_estimates),
                       estimates = object.var$range_estimates,
                       lower = object.var$range_lower,
                       upper = object.var$range_upper,
                       estimand = "Range")
    tab2 <- data.frame(attribute = names(object.var$variability_estimates),
                       estimates = k * object.var$variability_estimates,
                       lower = k * object.var$variability_lower,
                       upper = k * object.var$variability_upper,
                       estimand = "Variability")
    return(rbind(tab1, tab2))
  }
  
  # Regular
  var_acp <- conjacp.var(results_acp,
                         cacp = "conditional",
                         alpha = .05,
                         nsimul = 1000)
  table_var_acp <- var.table(var_acp)
  table_var_acp$type <- "Regular"
  
  # Conditional: education
  var_condEd <- conjacp.var(results_condEd,
                              cacp = "conditional",
                              alpha = .05,
                              nsimul = 1000)
  table_var_condEd <- var.table(var_condEd)
  table_var_condEd$type <- "Conditional on education"
  
  # Conditional: job
  var_condJob <- conjacp.var(results_condJob,
                            cacp = "conditional",
                            alpha = .05,
                            nsimul = 1000)
  table_var_condJob <- var.table(var_condJob)
  table_var_condJob$type <- "Conditional on occupation"
  
  # Conditional: country
  var_condCountry <- conjacp.var(results_condCountry,
                             cacp = "conditional",
                             alpha = .05,
                             nsimul = 1000)
  table_var_condCountry <- var.table(var_condCountry)
  table_var_condCountry$type <- "Conditional on country of origin"
  
  # Conditional: reason
  var_condReason <- conjacp.var(results_condReason,
                                 cacp = "conditional",
                                 alpha = .05,
                                 nsimul = 1000)
  table_var_condReason <- var.table(var_condReason)
  table_var_condReason$type <- "Conditional on reason for application"
  
table_var <- rbind(table_var_acp,
                   table_var_condEd,
                   table_var_condJob,
                   table_var_condCountry,
                   table_var_condReason)
levels(table_var$attribute) <- c("Origin", "Education", "Job Experience", "Gender", "Job",
                                 "Language Skills", "Job Plans", "Reason for Application",
                                 "Prior Trips to the U.S.")

# Plot graph

  # Conditional on country of origin and reason for application
  graph_var_countryreason <- ggplot(table_var[table_var$type %in% c("Conditional on country of origin",
                                                                    "Conditional on reason for application"),],
                                    aes(y = estimates, x = attribute, group = estimand)) +
    coord_flip(ylim = c(0, .43)) +
    geom_pointrange(aes(ymin = lower, ymax = upper, shape = estimand, color = estimand, fill = estimand),
                    position = position_dodge(width = .5), size = .2) +
    labs(y = "", x = "") +
    facet_grid(. ~ type, labeller = label_wrap_gen(width = 30)) +
    theme_fg(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box.margin = margin(-20, 0, 0, 0))  +
    scale_shape_manual(values = c(21, 22, 23, 23), name = "") +
    scale_fill_manual(values = cbPalette, name = "") +
    scale_colour_manual(values = cbPalette, name = "")
  
  # Conditional on occupation and education
  graph_var_jobeducation <- ggplot(table_var[table_var$type %in% c("Conditional on occupation",
                                                                   "Conditional on education"),],
                                    aes(y = estimates, x = attribute, group = estimand)) +
    coord_flip(ylim = c(0, .43)) +
    geom_pointrange(aes(ymin = lower, ymax = upper, shape = estimand, color = estimand, fill = estimand),
                    position = position_dodge(width = .5), size = .2) +
    labs(y = "", x = "") +
    facet_grid(. ~ type, labeller = label_wrap_gen(width = 30)) +
    theme_fg(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box.margin = margin(-20, 0, 0, 0))  +
    scale_shape_manual(values = c(21, 22, 23, 23), name = "") +
    scale_fill_manual(values = cbPalette, name = "") +
    scale_colour_manual(values = cbPalette, name = "")
  
  (graph_var_countryreason / graph_var_jobeducation) +
    plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  ggsave(filename = "Output/relimp_cond.pdf",
         height = 6, width = 6, device = cairo_pdf)
  
  # Regular
  graph_var_regular <- ggplot(table_var[table_var$type == "Regular",],
                                   aes(y = estimates, x = attribute, group = estimand)) +
    coord_flip(ylim = c(0, .43)) +
    geom_pointrange(aes(ymin = lower, ymax = upper, shape = estimand, color = estimand, fill = estimand),
                    position = position_dodge(width = .5), size = .2) +
    labs(y = "", x = "") +
    theme_fg(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.box.margin = margin(-20, 0, 0, 0))  +
    scale_shape_manual(values = c(21, 22, 23, 23), name = "") +
    scale_fill_manual(values = cbPalette, name = "") +
    scale_colour_manual(values = cbPalette, name = "")
  ggsave(graph_var_regular, filename = "Output/relimp_reg.pdf",
         height = 3.1, width = 3.9, device = cairo_pdf)
