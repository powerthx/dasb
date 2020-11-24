library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)

# Read FSI Excel into Tibble
file.list <- list.files(path = "/home/tarnold/R/1PrjFINAL/data/",pattern='*.xlsx')
df.list <- lapply(paste("/home/tarnold/R/1PrjFINAL/data/", file.list, sep=""), read_excel)

# Combine Array into DS
data_fsi_index <- rbind(df.list[[1]], df.list[[2]], df.list[[3]], df.list[[4]], df.list[[5]], df.list[[6]], df.list[[7]], df.list[[8]], df.list[[9]], df.list[[10]], df.list[[11]])

# Load ISO/UN Codes for Countries arround the World
iso_codes <- read_csv("/home/tarnold/R/1PrjFINAL/data/nationsonline_country_code_list.csv",
                      col_types = cols(
                        Row = col_integer(),
                        Country = col_character(),
                        ISO2 = col_character(),
                        ISO3 = col_character(),
                        UN = col_character()
                      ))
names(iso_codes) <- c("Row","Country", "ISO2", "ISO3", "UN")

# Load Missing ISO/UN Codes from additional CSV
mapping_missing_iso_codes <- read_csv("/home/tarnold/R/1PrjFINAL/data/mapping_missing_iso_codes.csv",
                                      col_types = cols(
                                        Country = col_character(),
                                        Alpha2 = col_character(),
                                        Alpha3Code = col_character(),
                                        UNCode = col_double()
                                      ))
names(mapping_missing_iso_codes) <- c("Country", "ISO2", "ISO3", "UN")

# Remove unused Row Column
iso_codes = subset(iso_codes, select = -c(Row) )

# Append missing ISO/UN Codes to DS
iso_codes <- rbind(iso_codes,mapping_missing_iso_codes)
# Remove dupplicates by Group By
iso_codes <- iso_codes %>% group_by(Country,ISO2,ISO3,UN) %>% summarise()

# Join ISO Codes to IFS Dataset
head(data_fsi_index)
head(iso_codes)
merged_table <- left_join(data_fsi_index,iso_codes,by=c("Country"="Country"))

# Checking if NA Join Values disappeared - OK
data_fsi_index <- merged_table %>% 
  filter(is.na(UN)) %>% group_by(Country) %>% summarise()

# Generate overview of NA Values in Dataset - OK - 0 Rows
data_fsi_index <- as_tibble(merged_table)
data_fsi_index = subset(data_fsi_index, select = c(`Country`, `UN`) )
data_fsi_index <- data_fsi_index %>% 
  filter(is.na(UN)) %>% group_by(Country) %>% summarise()

# Reload Dataset 
data_fsi_index <- as_tibble(merged_table)

# Transform Dataset Year and Rank to Integer Value
data_fsi_index <- data_fsi_index %>%
  mutate(
    Country = as.character(Country),  
    Year = substr(as.character(Year),1,4),
    Rank = as.integer(str_replace(str_replace(str_replace(str_replace(str_replace(as.character(Rank),'th', ""),'st', ""),'nd', ""),'rd', ""),'n/r',"")),
    Total = as.double(Total),
    `C1: Security Apparatus` = as.double(`C1: Security Apparatus`),
    `C2: Factionalized Elites` = as.double(`C2: Factionalized Elites`),
    `C3: Group Grievance`  = as.double(`C3: Group Grievance`),
    `E1: Economy`  = as.double(`E1: Economy`),
    `E2: Economic Inequality`  = as.double(`E2: Economic Inequality`),
    `E3: Human Flight and Brain Drain`  = as.double(`E3: Human Flight and Brain Drain`),
    `P1: State Legitimacy`  = as.double(`P1: State Legitimacy`),
    `P2: Public Services`  = as.double(`P2: Public Services`),
    `P3: Human Rights`  = as.double(`P3: Human Rights`),
    `S1: Demographic Pressures`  = as.double(`S1: Demographic Pressures`),
    `S2: Refugees and IDPs`  = as.double(`S2: Refugees and IDPs`),
    `X1: External Intervention`  = as.double(`X1: External Intervention`),
    ISO2  = as.character(ISO2),
    ISO3  = as.character(ISO3),
    UN  = as.character(UN))

# Export combined FSI Index to transform folder
write.csv(data_fsi_index,"/home/tarnold/R/1PrjFINAL/transformed/dataset_fsi_index.csv", row.names = TRUE)
