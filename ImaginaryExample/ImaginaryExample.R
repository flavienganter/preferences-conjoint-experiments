# Plot AMCEs and ACPs for the two-attribute fictional example
# Author: Flavien Ganter
# Created on December 29, 2019; last modified on October 1, 2020



### PRELIMINARIES ###########################################################


rm(list = ls())


## Working directory
setwd("")


## Packages
library(ggplot2)


## Graph theme add-on
extrafont::loadfonts()
source("../ImmigrantExperiment/theme_donors.R")
cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#56B4E9", "#F0E442")



### DATA ####################################################################


## Empty table to store results
table_plot <- data.frame(var = rep(c(NA, rep("Gender", 2), NA, NA, rep("Education", 7)), 2),
                         modality = rep(c("Gender:", "   Female", "   Male", "", "Education:",
                                          "   No formal", "   4th grade", "   8th grade",
                                          "   High school", "   Two-year college",
                                          "   College degree", "   Graduate degree"), 2),
                         estimate = NA,
                         type = c(rep("ACP", 12), rep("AMCE", 12)))
table_plot$modality <- factor(table_plot$modality,
                              levels = unique(table_plot$modality)[length(table_plot$modality):1])


## Parameters

# Gender
gender <- -.13

# Education
ed12 <- -.05
ed13 <- -.02
ed14 <- -.12
ed15 <- -.18
ed16 <- -.17
ed17 <- -.18
ed23 <- -.01
ed24 <- -.04
ed25 <- -.11
ed26 <- -.09
ed27 <- -.12
ed34 <- -.05
ed35 <- -.09
ed36 <- -.09
ed37 <- -.08
ed45 <- -.15
ed46 <- -.15
ed47 <- -.10
ed56 <- .01
ed57 <- 0
ed67 <- .01


## Quantities of interest

# AMCE
table_plot$estimate[2] <- gender
table_plot$estimate[3] <- -gender
table_plot$estimate[6] <- 1/6 * (ed12 + ed13 + ed14 + ed15 + ed16 + ed17)
table_plot$estimate[7] <- 1/6 * (- ed12 + ed23 + ed24 + ed25 + ed26 + ed27)
table_plot$estimate[8] <- 1/6 * (- ed13 - ed23 + ed34 + ed35 + ed36 + ed37)
table_plot$estimate[9] <- 1/6 * (- ed14 - ed24 - ed34 + ed45 + ed46 + ed47)
table_plot$estimate[10] <- 1/6 * (- ed17 - ed27 - ed37 - ed47 - ed57 - ed67)
table_plot$estimate[11] <- 1/6 * (- ed16 - ed26 - ed36 - ed46 - ed56 + ed67)
table_plot$estimate[12] <- 1/6 * (- ed15 - ed25 - ed35 - ed45 + ed56 + ed57)

# amces
table_plot$estimate[14] <- 0
table_plot$estimate[15] <- - 2 * gender * 1/2
table_plot$estimate[18] <- 0
table_plot$estimate[19] <- 1/6 * ((- ed12 + ed23 + ed24 + ed25 + ed26 + ed27) -
                                    (ed12 + ed13 + ed14 + ed15 + ed15 + ed17)) * 6/7
table_plot$estimate[20] <- 1/6 * ((- ed13 - ed23 + ed34 + ed35 + ed36 + ed37) -
                                    (ed12 + ed13 + ed14 + ed15 + ed15 + ed17)) * 6/7
table_plot$estimate[21] <- 1/6 * ((- ed14 - ed24 - ed34 + ed45 + ed46 + ed47) -
                                    (ed12 + ed13 + ed14 + ed15 + ed15 + ed17)) * 6/7
table_plot$estimate[22] <- 1/6 * ((- ed17 - ed27 - ed37 - ed47 - ed57 - ed67) -
                                    (ed12 + ed13 + ed14 + ed15 + ed15 + ed17)) * 6/7
table_plot$estimate[23] <- 1/6 * ((- ed16 - ed26 - ed36 - ed46 - ed56 + ed67) -
                                    (ed12 + ed13 + ed14 + ed15 + ed15 + ed17)) * 6/7
table_plot$estimate[24] <- 1/6 * ((- ed15 - ed25 - ed35 - ed45 + ed56 + ed57) -
                                    (ed12 + ed13 + ed14 + ed15 + ed15 + ed17)) * 6/7



### PLOT ####################################################################

table_plot$estimate <- ifelse(is.na(table_plot$estimate), 1, table_plot$estimate)
hline <- data.frame(type = c("AMCE", "ACP"), yint = c(0, 5))

plot_fictional <- ggplot(table_plot, aes(y = estimate, x = modality)) +
  coord_flip(ylim = c(-.22, .2)) +
  geom_hline(data = hline, aes(yintercept = yint), size = .1, colour = "black") +
  geom_point() +
  labs(y = "", x = "") +
  facet_grid(. ~ type) +
  theme_donors(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        axis.text.y = element_text(hjust = 0 , vjust = .5 ),
        legend.position = "bottom",
        legend.box.margin = margin(-20, 0, 0, 0))  +
  scale_shape_manual(values = c(21, 22, 23, 23), name = "") +
  scale_fill_manual(values = cbPalette, name = "") +
  scale_colour_manual(values = cbPalette, name = "")

# table a2
tablea2 <- data.frame(modality = table.fig3[table.fig3$type == "AMCE",]$modality,
                      amce = table.fig3[table.fig3$type == "AMCE",]$estimate,
                      amcep = table.fig3[table.fig3$type == "AMCEP",]$estimate)
tablea2$amce <- ifelse(tablea2$amce %in% c(0,1), NA, tablea2$amce)
tablea2$amcep <- ifelse(tablea2$amcep %in% c(0,1), NA, tablea2$amcep)
