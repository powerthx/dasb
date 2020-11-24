library(tidyverse)
library(modelr)
# Paths to the Datasets
xexport_ds_path<- "/home/tarnold/R/1PrjFINAL/transformed/dataset_internet_for_prosperity.csv"
dataset_fsi_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_fsi_index.csv"
# The list of almost valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "FBS",
                    "Fixed-telephone subscriptions" = "FTS",
                    "Mobile-cellular telephone subscriptions" = "MTS",
                    "Percentage of Individuals using the Internet" = "PUI",
                    "ICT Index by UN Data" = "ICT",
                    "Fragile State Index" = "FSI")
## Read itu data source
dataset_itu_index <- read_csv(xexport_ds_path, 
                              col_types = cols(X1 = col_skip(),
                                               "Country" = col_character(), 
                                               "Year" = col_character(), 
                                               "ISO3" = col_character(), #FOCUS COUNTRIES "AFG","ALB","DZA","AGO","ATG","ARG","ARM","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BTN","BOL","BIH","BWA","BRA","BRN","BGR","BFA","BDI","KHM","CMR","CAN","CPV","CAF","TCD","CHL","CHN","COL","COM","COD","COG","CRI","CIV","HRV","CUB","CYP","CZE","PRK","DNK","DJI","DOM","ECU","EGY","SLV","GNQ","ERI","EST","ETH","FJI","FIN","FRA","GAB","GMB","GEO","DEU","GHA","GRC","GRD","GTM","GIN","GNB","GUY","HTI","HND","HUN","ISL","IND","IDN","IRN","IRQ","IRL","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KOR","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LTU","LUX","MDG","MWI","MYS","MDV","MLI","MLT","MRT","MUS","MEX","FSM","MDA","MNG","MNE","MAR","MOZ","MMR","NAM","NPL","NLD","NZL","NIC","NER","NGA","NOR","OMN","PAK","PAN","PNG","PRY","PER","PHL","POL","PRT","QAT","ROU","RUS","RWA","WSM","STP","SAU","SEN","SRB","SYC","SLE","SGP","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","SDN","SUR","SWZ","SWE","CHE","SYR","TJK","TZA","MKD","THA","TLS","TGO","TTO","TUN","TUR","TKM","UGA","UKR","ARE","GBR","USA","URY","UZB","VEN","VNM","YEM","ZMB","ZWE",
                                               "FBS" = col_double(), 
                                               "FTS" = col_double(), 
                                               "PUI" = col_double(), 
                                               "MTS" = col_double(),
                                               "FSI" = col_double(),
                                               'economy' = col_factor(levels = c("1. Developed region: G7","2. Developed region: nonG7","3. Emerging region: BRIC","4. Emerging region: MIKT","5. Emerging region: G20","6. Developing region","7. Least developed region")),
                                               'grp' = col_factor(levels = c("1. High income: OECD","2. High income: nonOECD","3. Upper middle income","4. Lower middle income","5. Low income")),
                                               'region' = col_factor(levels = c("Africa","Americas","Antarctica","Asia","Europe","Oceania")), 
                                               'subregion' = col_factor(levels = c("Antarctica","Australia and New Zealand","Caribbean","Central America","Central Asia","Eastern Africa","Eastern Asia","Eastern Europe","Melanesia","Micronesia","Middle Africa","Northern Africa","Northern America","Northern Europe","Polynesia","Seven seas (open ocean)","South America","South-Eastern Asia","Southern Africa","Southern Asia","Southern Europe","Western Africa","Western Asia","Western Europe"))
                              ))
# Load data ----
ds <- dataset_itu_index
ds2 <- dataset_itu_index
ds2 <- as.data.frame(ds2)
summary(ds2) # summary

get_data <- function(xeconomy, xgrp, xregion, xSTB, xYear) {
  input_data <- data.frame(
    economy = c(xeconomy), grp = c(xgrp), region = c(xregion), STB = c(xSTB), Year = c(xYear)
  )
  
  # Model to predict FBS Score
  fbs_model <- lm(FBS ~ economy + grp + region + STB + Year,data=ds2)
  summary(fbs_model)
  fbs_model
  # Model to predict FTS Score
  fts_model <- lm(FTS ~ grp + region + STB + Year,data=ds2)
  summary(fts_model)
  # Model to predict MTS Score
  mts_model <- lm(MTS ~ grp + region + STB + Year,data=ds2)
  # Model to predict FSI Score
  fsi_model <- lm(FSI ~ grp + region + STB + Year,data=ds2)
  
  # Prediction
  output_fbs <- round(predict(fbs_model, newdata = input_data), digits = 0)
  output_fts <- round(predict(fts_model, newdata = input_data), digits = 0)
  output_mts <- round(predict(mts_model, newdata = input_data), digits = 0)
  output_fsi <- round(predict(fsi_model, newdata = input_data), digits = 0)
  #create result
  result <- c(output_fbs, output_fts, output_mts, output_fsi)
  
  # return the result
  return(result)
}
