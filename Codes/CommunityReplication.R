# Replicate the analyses of the community experiment (Mummolo and Nall 2017)
# using the ACP framework (Ganter 2021)
# Author: Flavien Ganter
# Created on February 23, 2021; last modified on July 16, 2021



#### PRELIMINARIES ####

# Clear working space
rm(list = ls())

# Set working directory
setwd("") ### specify path to root replication folder

# Packages and functions
library(tidyverse)
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
load("Data/MummoloNall.RData")

# Select relevant variables
data_long <- conj.data.party[!is.na(conj.data.party$response),
                             c("partywlean", "respid", "task",
                               "conj.school", "conj.place", "response")]

# Convert attributes into factors
data_long$conj.school  <- as.factor(data_long$conj.school)
data_long$conj.place   <- as.factor(data_long$conj.place)

# Get attribute labels and relevel
levels(data_long$conj.school) <- c("5/10", "9/10")
levels(data_long$conj.place)  <- c("City – more residential area", "City – downtown area", 
                                  "Rural area", "Small town", "Suburban – only houses", 
                                  "Suburban – downtown area")
data_long$conj.place          <- relevel(data_long$conj.place, "City – downtown area")

# Create task variable
data_long         <- data_long[order(data_long$respid),]
data_long$RespID  <- group_indices(data_long, respid)
data_long$task_fg <- cumsum(!duplicated(data_long[, c("RespID", "task")]))

# Create subgroup variable
data_long$partywlean <- as.factor(data_long$partywlean)

# Create CONJACP object
conjacp_data <- conjacp.prepdata(response ~ conj.school + conj.place,
                                 data = data_long,
                                 tasks = "task_fg",
                                 subgroups = "partywlean",
                                 id = "RespID")



#### ACP & AMCE ####


## Estimate quantities of interest

# ACPs
results_acp <- conjacp.estimation(conjacp_data,
                                  estimand = "acp",
                                  adjust = FALSE)
  
# AMCEs
  
  # Create empty vectors to store results
  ind_estimate <- NULL
  ind_se       <- NULL
  
  # Loop over independently randomized attributes
  for (attribute in c("conj.school", "conj.place")) {
    
    # Create data frame for model
    data_model <- data_long[, c(attribute, "response")]
    names(data_model)[-ncol(data_model)] <- paste0(names(data_model)[-ncol(data_model)], ".")
    
    # Estimate model
    model <- lm(response ~ ., data = data_model)
    
    # Store estimates
    ind_estimate <- c(ind_estimate, coef(model)[-1])
    vcov         <- vcovCluster(model, data_long$RespID)
    ind_se       <- c(ind_se, sqrt(diag(vcov))
                      [2:(nlevels(as.data.frame(data_long)[, match(attribute, colnames(data_long))]))])
    
  }

    
## Graph ACPs vs. AMCEs
    
# Prepare estimate table
table_acpamce <- data.frame(modality = c("School quality:", "   5/10", "   9/10", "", "Type of place:",
                                         "   City – downtown area", "   City – more residential area", 
                                         "   Rural area", "   Small town", "   Suburban – only houses", 
                                         "   Suburban – downtown area", "School quality:", "   5/10", "   9/10", "", "Type of place:",
                                         "   City – downtown area", "   City – more residential area", 
                                         "   Rural area", "   Small town", "   Suburban – only houses", 
                                         "   Suburban – downtown area"),
                            var = c("", "School quality", "School quality", "", "",
                                    "Type of place", "Type of place", 
                                    "Type of place", "Type of place", "Type of place", 
                                    "Type of place", "", "School quality", "School quality", "", "",
                                    "Type of place", "Type of place", 
                                    "Type of place", "Type of place", "Type of place", 
                                    "Type of place"),
                            estimate = c(1, results_acp$estimates[1:2], 1, 1, results_acp$estimates[3:8],
                                         1, 0, ind_estimate[1], 1, 1, 0, ind_estimate[2:6]),
                            se = c(0, sqrt(diag(results_acp$vcov))[1:2], 0, 0, sqrt(diag(results_acp$vcov))[3:8],
                                   0, 0, ind_se[1], 0, 0, 0, ind_se[2:6]),
                            type = c(rep("ACP", 11), rep("AMCE", 11)))
table_acpamce$modality <- factor(table_acpamce$modality, levels = unique(table_acpamce$modality)[length(table_acpamce$modality):1])
hline <- data.frame(type = c("AMCE", "ACP"), yint = c(0, 5))

# Plot graphs
commplot <- ggplot(table_acpamce, aes(y = estimate, x = modality, group = type)) +
  coord_flip(ylim = c(-.22, .2)) +
  geom_hline(data = hline, aes(yintercept = yint), size = .1, colour = "black") +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se,
                      color = type, shape = type, fill = type),
                  position = position_dodge(width = .5), size = .2) +
  labs(y = "", x = "") +
  facet_grid(. ~ type) +
  theme_fg(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        axis.text.y = element_text(hjust = 0 , vjust = .5 ),
        legend.position = "none")  +
  scale_shape_manual(values = c(21, 22, 23, 23), name = "") +
  scale_fill_manual(values = cbPalette, name = "") +
  scale_colour_manual(values = cbPalette, name = "")
ggsave(commplot, filename = "Output/acpamce_comm.pdf",
       height = 2.8, width = 6, device = cairo_pdf)
