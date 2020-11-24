library(dplyr)
library(readr)
library(ggplot2) # tidyverse data visualization package
library(tidyr)

dataset_fsi_index <- read_csv("/home/tarnold/R/1PrjFINAL/transformed/dataset_fsi_index.csv", 
                                col_types = cols(`C1: Security Apparatus` = col_double(), 
                                                 `C2: Factionalized Elites` = col_double(), 
                                                 `C3: Group Grievance` = col_double(), 
                                                 Country = col_character(), `E1: Economy` = col_double(), 
                                                 `E2: Economic Inequality` = col_double(), 
                                                 `E3: Human Flight and Brain Drain` = col_double(), 
                                                 ISO2 = col_character(), ISO3 = col_character(), 
                                                 `P1: State Legitimacy` = col_double(), 
                                                 `P2: Public Services` = col_double(), 
                                                 `P3: Human Rights` = col_double(), 
                                                 Rank = col_integer(), Row = col_skip(), 
                                                 `S1: Demographic Pressures` = col_double(), 
                                                 `S2: Refugees and IDPs` = col_double(), 
                                                 Total = col_double(), UN = col_character(), 
                                                 `X1: External Intervention` = col_double(), 
                                                 Year = col_double()))

dataset_fsi_index_wide <- dataset_fsi_index %>%
    pivot_wider(names_from = Year, values_from = c(Total,Rank,
                                                   `C1: Security Apparatus`,
                                                   `C2: Factionalized Elites`,
                                                   `C3: Group Grievance`,
                                                   `E1: Economy`,
                                                   `E2: Economic Inequality`,
                                                   `E3: Human Flight and Brain Drain`,
                                                   `P1: State Legitimacy`,
                                                   `P2: Public Services`,
                                                   `P3: Human Rights`,
                                                   `S1: Demographic Pressures`,
                                                   `S2: Refugees and IDPs`,
                                                   `X1: External Intervention`))
  
write.csv(dataset_fsi_index_wide,"/home/tarnold/R/1PrjFINAL/transformed/dataset_fsi_index_wide.csv", row.names = TRUE)

