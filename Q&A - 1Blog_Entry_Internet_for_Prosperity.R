library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(RColorBrewer)

# Paths to the Datasets
dataset_itu_index_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index_values.csv"
dataset_fsi_index_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_fsi_index.csv"
metadata_countries_path <- "/home/tarnold/R/1PrjFINAL/data/metadata_countries.csv"

# The list of almost valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "FBS",
                    "Fixed-telephone subscriptions" = "FTS",
                    "Mobile-cellular telephone subscriptions" = "MTS",
                    "Active mobile-broadband subscriptions" = "AMB",
                    "Percentage of Individuals using the Internet" = "PUI",                                        
                    "Individuals using the Internet" = "IUI")

## Read itu data source
dataset_itu_index <- read_csv(dataset_itu_index_path, 
                             col_types = cols("X1" = col_skip(),
                                              "Country" = col_character(), 
                                              "Year" = col_double(), 
                                              "ISO2" = col_character(), 
                                              "ISO3" = col_character(), #FOCUS COUNTRIES "AFG","ALB","DZA","AGO","ATG","ARG","ARM","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BTN","BOL","BIH","BWA","BRA","BRN","BGR","BFA","BDI","KHM","CMR","CAN","CPV","CAF","TCD","CHL","CHN","COL","COM","COD","COG","CRI","CIV","HRV","CUB","CYP","CZE","PRK","DNK","DJI","DOM","ECU","EGY","SLV","GNQ","ERI","EST","ETH","FJI","FIN","FRA","GAB","GMB","GEO","DEU","GHA","GRC","GRD","GTM","GIN","GNB","GUY","HTI","HND","HUN","ISL","IND","IDN","IRN","IRQ","IRL","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KOR","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LTU","LUX","MDG","MWI","MYS","MDV","MLI","MLT","MRT","MUS","MEX","FSM","MDA","MNG","MNE","MAR","MOZ","MMR","NAM","NPL","NLD","NZL","NIC","NER","NGA","NOR","OMN","PAK","PAN","PNG","PRY","PER","PHL","POL","PRT","QAT","ROU","RUS","RWA","WSM","STP","SAU","SEN","SRB","SYC","SLE","SGP","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","SDN","SUR","SWZ","SWE","CHE","SYR","TJK","TZA","MKD","THA","TLS","TGO","TTO","TUN","TUR","TKM","UGA","UKR","ARE","GBR","USA","URY","UZB","VEN","VNM","YEM","ZMB","ZWE",
                                              "UN" = col_character(), 
                                              "FBS" = col_double(), 
                                              "FTS" = col_double(), 
                                              "PUI" = col_double(), 
                                              "MTS" = col_double(), 
                                              "AMB" = col_double(), 
                                              "IUI" = col_double()
                             ))
# Remove aggregated Data Rows
dataset_itu_index <- dataset_itu_index %>%
  filter(`Country` %in% c("World","Developing","Asia & Pacific","Developed","The Americas","Europe","Africa","LDCs","Arab States","CIS","Ascension") == FALSE)

# Load FSI Index
dataset_fsi_index <- read_csv(dataset_fsi_index_path, 
                              col_types = cols(X1 = col_skip(),
                                               ISO3 = col_character(), 
                                               Total = col_double(), 
                                               Year = col_double()))
# Select relevant Columns from FSI Index
dataset_fsi_index <- dataset_fsi_index %>%
  select(Year, Total, ISO3)
namesOfColumns <- c("Year", "FSI", "ISO3")
colnames(dataset_fsi_index) <- namesOfColumns

# Get additional attributes from the naturalearthdata project
metadata_countries <- read_csv(metadata_countries_path, 
                               col_types = cols(
                                 'iso_a3' = col_character(),
                                 'economy' = col_factor(levels = c("1. Developed region: G7","2. Developed region: nonG7","3. Emerging region: BRIC","4. Emerging region: MIKT","5. Emerging region: G20","6. Developing region","7. Least developed region")),
                                 'grp' = col_factor(levels = c("1. High income: OECD","2. High income: nonOECD","3. Upper middle income","4. Lower middle income","5. Low income")),
                                 'region' = col_factor(levels = c("Africa","Americas","Antarctica","Asia","Europe","Oceania","Seven seas (open ocean)")), 
                                 'subregion' = col_factor(levels = c("Antarctica","Australia and New Zealand","Caribbean","Central America","Central Asia","Eastern Africa","Eastern Asia","Eastern Europe","Melanesia","Micronesia","Middle Africa","Northern Africa","Northern America","Northern Europe","Polynesia","Seven seas (open ocean)","South America","South-Eastern Asia","Southern Africa","Southern Asia","Southern Europe","Western Africa","Western Asia","Western Europe"))
                                                          ))

metadata_countries <- metadata_countries %>%
  select(iso_a3, economy, grp,region, subregion)
namesOfColumns <- c("ISO3", "economy", "grp", "region", "subregion")
colnames(metadata_countries) <- namesOfColumns

# For EDA we only consider countires which are having a FSI Index
# This is sad because our data shows that a lot of countries like Monaco and Gibraltar have alot 
# of insight and stories to tell. Tax haven countries in particular look very well connected to the internet!
# Can we follow revenue streams and we will find internet coverMTS as well?!
itu <- inner_join(dataset_itu_index, dataset_fsi_index,by = c("ISO3" = "ISO3", "Year" = "Year"))
itu <- left_join(itu, metadata_countries,  by =c("ISO3" = "ISO3") )



# Remove unneeded columns
da <- ds
ds = subset(ds, select = c("FBS","FTS","PUI","MTS","FSI","C1", "C2", "C3", "E1", "E2", "E3","P1", "P2", "P3", "S1", "S2", "X1") )
#Remove NA Values
ds <- ds %>%
  drop_na()

datasetDecade <- ds
dataset.corr <- cor(ds)

