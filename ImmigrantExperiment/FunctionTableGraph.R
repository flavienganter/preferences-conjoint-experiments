# Function that generates tables used to produce the coefficient plots
# Flavien Ganter
# Created on July 26, 2019; last modified on August 26, 2020

table.graph <- function(data,
                        estimand = "acp",
                        cacp = "conditional",
                        condition = NULL
) {
  require(stringr)
  
  if (estimand %in% c("amce", "dsp")) {
    estimates <- data$estimates
    se        <- data$se
  } else if (cacp == "conditional") {
    estimates <- data$estimates
    se        <- sqrt(diag(data$vcov))
  } else if (cacp == "all_comparable") {
    estimates <- data$estimates_alt
    se        <- sqrt(diag(data$vcov_alt))
  }
  
  # Create empty table to fill in
  out_table <- data.frame(var      = as.factor(str_extract(names(estimates), ".+?(?=\\.)")),
                          modality = NA,
                          estimate = estimates,
                          se       = se)
    
  if (estimand == "full_cond_acp") {
    if (condition == "FeatEd") {
      out_table_ed <- data.frame(var = rep("FeatEd", 10),
                                 modality = NA,
                                 estimate = NA,
                                 se = NA)
      out_table <- rbind(out_table, out_table_ed)
      out_table$var <- factor(out_table$var, levels = c("FeatCountry", "FeatEd", "FeatExp", "FeatGender", "FeatJob", "FeatLang", 
                                                        "FeatPlans", "FeatReason", "FeatTrips"))
    } else if (condition == "FeatJob") {
      out_table_job <- data.frame(var = rep("FeatJob", 18),
                                 modality = NA,
                                 estimate = NA,
                                 se = NA)
      out_table <- rbind(out_table, out_table_job)
      out_table$var <- factor(out_table$var, levels = c("FeatCountry", "FeatEd", "FeatExp", "FeatGender", "FeatJob", "FeatLang", 
                                                        "FeatPlans", "FeatReason", "FeatTrips"))
    } else if (condition == "FeatCountry") {
      out_table_country <- data.frame(var = rep("FeatCountry", 14),
                                  modality = NA,
                                  estimate = NA,
                                  se = NA)
      out_table <- rbind(out_table, out_table_country)
      out_table$var <- factor(out_table$var, levels = c("FeatCountry", "FeatEd", "FeatExp", "FeatGender", "FeatJob", "FeatLang", 
                                                        "FeatPlans", "FeatReason", "FeatTrips"))
    } else if (condition == "FeatReason") {
      out_table_reason <- data.frame(var = rep("FeatReason", 5),
                                      modality = NA,
                                      estimate = NA,
                                      se = NA)
      out_table <- rbind(out_table, out_table_reason)
      out_table$var <- factor(out_table$var, levels = c("FeatCountry", "FeatEd", "FeatExp", "FeatGender", "FeatJob", "FeatLang", 
                                                        "FeatPlans", "FeatReason", "FeatTrips"))
    }
  }

  # Variable labelsabels - variables
  levels(out_table$var) <- c("Origin",
                             "Education",
                             "Job Experience",
                             "Gender",
                             "Job",
                             "Language Skills",
                             "Job Plans",
                             "Reason for Application",
                             "Prior Entry")
  
  # Add reference levels
  if (estimand == "amce") {
    out_table <- rbind(c("Gender", NA, 0, 0),
                       out_table[out_table$var == "Gender",],
                       c("Education", NA, 0, 0),
                       out_table[out_table$var == "Education",],
                       c("Language Skills", NA, 0, 0),
                       out_table[out_table$var == "Language Skills",],
                       c("Origin", NA, 0, 0),
                       out_table[out_table$var == "Origin",],
                       c("Job", NA, 0, 0),
                       out_table[out_table$var == "Job",],
                       c("Job Experience", NA, 0, 0),
                       out_table[out_table$var == "Job Experience",],
                       c("Job Plans", NA, 0, 0),
                       out_table[out_table$var == "Job Plans",],
                       c("Reason for Application", NA, 0, 0),
                       out_table[out_table$var == "Reason for Application",],
                       c("Prior Entry", NA, 0, 0),
                       out_table[out_table$var == "Prior Entry",])
  }
  
  # Modality labels
  offset <- c("   ")
  out_table$modality[out_table$var == "Gender"] <- paste(offset, c("Female", "Male"))
  out_table$modality[out_table$var == "Education"] <- paste(offset, c("High school", "No formal", "4th grade",
                                                                      "8th grade", "Two-year college",
                                                                      "College degree", "Graduate degree"))
  out_table$modality[out_table$var == "Language Skills"] <- paste(offset, c("Broken English", "Fluent English",
                                                                            "Tried English but unable",
                                                                            "Used interpreter"))
  out_table$modality[out_table$var == "Origin"] <- paste(offset, c("India", "Germany", "France", "Mexico",
                                                                   "Philippines", "Poland", "China", "Sudan",
                                                                   "Somalia", "Iraq"))
  out_table$modality[out_table$var == "Job"] <- paste(offset, c("Teacher", "Janitor", "Waiter",
                                                                "Child care provider", "Gardener",
                                                                "Financial analyst", "Construction worker",
                                                                "Computer programmer", "Nurse",
                                                                "Research scientist", "Doctor"))
  out_table$modality[out_table$var == "Reason for Application"] <- paste(offset, c("Reunite with family",
                                                                                   "Seek better job",
                                                                                   "Escape persecution"))
  out_table$modality[out_table$var == "Job Experience"] <- paste(offset, c("1-2 years", "None", "3-5 years",
                                                                           "5+ years"))
  out_table$modality[out_table$var == "Job Plans"] <- paste(offset, c("Will look for work",
                                                                      "Contract with employer",
                                                                      "Interviews with employer",
                                                                      "No plans to look for work"))
  out_table$modality[out_table$var == "Prior Entry"] <- paste(offset, c("Never", "Once as tourist",
                                                                        "Many times as tourist",
                                                                        "Six months with family",
                                                                        "Once w/o authorization"))
  
  # Attribute labels and order
  order_mod <- data.frame(modality = c("    Male", "    Female", "    None", "    1-2 years", "    3-5 years", 
                                       "    5+ years", "    No plans to look for work", "    Will look for work", 
                                       "    Interviews with employer", "    Contract with employer", 
                                       "    Once w/o authorization", "    Never", "    Once as tourist", 
                                       "    Many times as tourist", "    Six months with family", "    Used interpreter", 
                                       "    Tried English but unable", "    Broken English", "    Fluent English", 
                                       "    No formal", "    4th grade", "    8th grade", "    High school", 
                                       "    Two-year college", "    College degree", "    Graduate degree", 
                                       "    Janitor", "    Waiter", "    Child care provider", "    Gardener", 
                                       "    Financial analyst", "    Construction worker", "    Computer programmer", 
                                       "    Teacher", "    Nurse", "    Research scientist", "    Doctor", 
                                       "    Iraq", "    Somalia", "    Sudan", "    China", "    India", 
                                       "    Poland", "    Philippines", "    Mexico", "    France", 
                                       "    Germany", "    Seek better job", "    Reunite with family", 
                                       "    Escape persecution"),
                          order = 1:50)
  out_table <- merge(out_table, order_mod, by = "modality")
  out_table_var <- data.frame(modality = c("Gender:", " ",
                                           "Job Experience:", "  ",
                                           "Job Plans:", "   ",
                                           "Prior Trips to the U.S.:", "    ",
                                           "Language Skills:", "     ",
                                           "Education:", "      ",
                                           "Job:", "       ",
                                           "Origin:", "        ",
                                           "Reason for Application:"),
                              order = c(.5, 2.1, 2.5, 6.1, 6.5, 10.1, 10.5, 15.1, 15.5, 19.1, 19.5, 
                                        26.1, 26.5, 37.1, 37.5, 47.1, 47.5),
                              estimate = 1,
                              se = 0,
                              var = NA)
  out_table <- rbind(out_table, out_table_var)
  out_table <- out_table[order(out_table$order),]
  out_table$modality <- factor(out_table$modality, levels = unique(out_table$modality)[length(out_table$modality):1])
  
  return(out_table)
  
}
