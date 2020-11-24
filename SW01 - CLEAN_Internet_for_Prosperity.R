library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)

# Paths to the Datasets
dataset_itu_index_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index_values.csv"
dataset_fsi_index_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_fsi_index.csv"
metadata_countries_path <- "/home/tarnold/R/1PrjFINAL/data/metadata_countries.csv"
xexport_ds_path<- "/home/tarnold/R/1PrjFINAL/transformed/dataset_internet_for_prosperity.csv"


# The list of almost valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "FBS",
                    "Fixed-telephone subscriptions" = "FTS",
                    "Mobile-cellular telephone subscriptions" = "MTS",
                    "Percentage of Individuals using the Internet" = "PUI"                                        
                    #"Active mobile-broadband subscriptions" = "AMB",
                    #"Individuals using the Internet" = "IUI"
                    )

## Read itu data source
dataset_itu_index <- read_csv(dataset_itu_index_path, col_types = cols("X1" = col_skip(),
                                              "Country" = col_character(), 
                                              "Year" = col_double(), 
                                              "ISO2" = col_character(), 
                                              "ISO3" = col_character(), #FOCUS COUNTRIES "AFG","ALB","DZA","AGO","ATG","ARG","ARM","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BTN","BOL","BIH","BWA","BRA","BRN","BGR","BFA","BDI","KHM","CMR","CAN","CPV","CAF","TCD","CHL","CHN","COL","COM","COD","COG","CRI","CIV","HRV","CUB","CYP","CZE","PRK","DNK","DJI","DOM","ECU","EGY","SLV","GNQ","ERI","EST","ETH","FJI","FIN","FRA","GAB","GMB","GEO","DEU","GHA","GRC","GRD","GTM","GIN","GNB","GUY","HTI","HND","HUN","ISL","IND","IDN","IRN","IRQ","IRL","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KOR","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LTU","LUX","MDG","MWI","MYS","MDV","MLI","MLT","MRT","MUS","MEX","FSM","MDA","MNG","MNE","MAR","MOZ","MMR","NAM","NPL","NLD","NZL","NIC","NER","NGA","NOR","OMN","PAK","PAN","PNG","PRY","PER","PHL","POL","PRT","QAT","ROU","RUS","RWA","WSM","STP","SAU","SEN","SRB","SYC","SLE","SGP","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","SDN","SUR","SWZ","SWE","CHE","SYR","TJK","TZA","MKD","THA","TLS","TGO","TTO","TUN","TUR","TKM","UGA","UKR","ARE","GBR","USA","URY","UZB","VEN","VNM","YEM","ZMB","ZWE",
                                              "UN" = col_character(), 
                                              "FBS" = col_double(), 
                                              "FTS" = col_double(), 
                                              "PUI" = col_double(), 
                                              "MTS" = col_double(), 
                                              "AMB" = col_skip(), 
                                              "IUI" = col_skip()))
# Remove aggregated Data Rows
dataset_itu_agg <- dataset_itu_index %>%
  filter(`Country` %in% c("World","Developing","Asia & Pacific","Developed","The Americas","Europe","Africa","LDCs"))

dataset_itu_index <- dataset_itu_index %>%
  filter(`Country` %in% c("World","Developing","Asia & Pacific","Developed","The Americas","Europe","Africa","LDCs","Arab States","CIS","Ascension") == FALSE)

# Load FSI Index
dataset_fsi_index <- read_csv(dataset_fsi_index_path, 
                              col_types = cols(X1 = col_skip(),ISO3 = col_character(), 
                                               Total = col_double(), 
                                               Year = col_double()))
# Select relevant Columns from FSI Index
dataset_fsi_index <- dataset_fsi_index %>%
  dplyr::select(Year, Total, ISO3)
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
  dplyr::select(iso_a3, economy, grp,region, subregion)
namesOfColumns <- c("ISO3", "economy", "grp", "region", "subregion")
colnames(metadata_countries) <- namesOfColumns

# For EDA we only consider countires which are having a FSI Index
# This is sad because our data shows that a lot of countries like Monaco and Gibraltar have alot 
# of insight and stories to tell. Tax haven countries in particular look very well connected to the internet!
# Can we follow revenue streams and we will find internet coverage as well?!
itu <- left_join(dataset_itu_index, dataset_fsi_index,by = c("ISO3" = "ISO3", "Year" = "Year"))
# Consider only countries with FSI Index / Way to perserve 2007 Year data for AVG Fix
itu <- itu %>% filter(ISO3 %in% dataset_fsi_index$ISO3)
itu <- left_join(itu, metadata_countries, by =c("ISO3" = "ISO3") )

# Adding Growth Rate Column
itu <- itu %>%
  dplyr::mutate(YearPrev = Year-1)

# Determine Joing Dataset
itu_select <- itu %>%
  dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI,FSI)

# Join Prev Year Data
itu <- left_join(itu, itu_select,  by =c("ISO3" = "ISO3", "YearPrev"="Year"),suffix = c("", "Prev") )

# Calculate Growth Rate (GR Columns)
itu <- itu %>%
  dplyr::mutate(FBSGR=(FBS-FBSPrev)/FBSPrev*100, FTSGR=(FTS-FTSPrev)/FTSPrev*100, MTSGR=(MTS-MTSPrev)/MTSPrev*100, FSIGR=(FSI-FSIPrev)/FSIPrev*100, PUIGR=PUI-PUIPrev,FSIGR=FSI-FSIPrev)

itu_select <- itu %>%
  dplyr::select(Country,Year,ISO2,ISO3,UN,FBS,FTS,MTS,PUI,FSI,FBSGR,FTSGR,MTSGR,PUIGR,FSIGR)

ituNA <- itu %>%
  dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI,FSI)

# Find NA Values in Dataset
ituNA <- ituNA %>%
  filter(between(Year, 2008, 2018)) %>% 
  filter_all(any_vars( is.na(.)))

# Count/Aggregate missing Values
aggNA <- aggregate(ituNA[,3:6], by=list(ituNA$ISO3), FUN=function(x) { sum(is.na(x)) }) %>% arrange(desc(FBS,FTS,MTS,PUI))

#NaNtc Rows not further to consider / to many values missing -> Removing Countries from DS
aggNaNtc <- aggNA  %>% filter(FBS>1 | FTS>1 | MTS>1 | PUI>1)

#Remove all Country Data Sets which not fullfill minimum QS Standards
aggNA <- anti_join(aggNA,aggNaNtc,by =c("Group.1" = "Group.1"))

#NAFix Rows to fix with avg. function (1. Method AVG(Next Year / Previous Year) / 2. Method (AVG(Next Year | Previous Year * AVG(GrowthRateOther9Years))))
aggNAFix <- aggNA %>% filter(FBS<=1 & FTS<=1 & MTS<=1 & PUI<=1 )

# Select Rows for fix
ituNAFix <- ituNA %>% filter(ISO3 %in% aggNAFix$Group.1)
ituNAFix <- ituNAFix %>% dplyr::mutate(YearPrev = Year-1,YearNext = Year+1)

# Prepare Dataset to fix 
ituNAFixPrev <- left_join(ituNAFix, itu_select,  by =c("ISO3" = "ISO3", "YearPrev"="Year"),suffix = c("", "Prev"), KEEP=TRUE )
ituNAFixNext <- left_join(ituNAFixPrev, itu_select,  by =c("ISO3" = "ISO3", "YearNext"="Year"),suffix = c("", "Next"), KEEP=TRUE )

# Phase 1
# Fix Rows by mean (Indicator Value: nextYear + previousYear = Fixed Value)
avgFix <- ituNAFixNext %>%
  filter(is.na(FTS) & is.na(FTSNext)==FALSE & is.na(FTSPrev)==FALSE | is.na(FBS) & is.na(FBSNext)==FALSE & is.na(FBSPrev)==FALSE | is.na(MTS) & is.na(MTSNext)==FALSE & is.na(MTSPrev)==FALSE | is.na(PUI) & is.na(PUINext)==FALSE & is.na(PUIPrev)==FALSE)

# AVG FIX mutation PHASE 1 -> 8 Rows in total we can fix
avgFixed <- avgFix %>% 
  mutate(FBS = ifelse(is.na(FBS), (FBSNext+FBSPrev)/2, FBS),
         FTS = ifelse(is.na(FTS), (FTSNext+FTSPrev)/2, FTS),
         MTS = ifelse(is.na(MTS), (MTSNext+MTSPrev)/2, MTS),
         PUI = ifelse(is.na(PUI), (PUINext+PUIPrev)/2, PUI))

delta.itu <- anti_join(itu,avgFixed,by =c("ISO3" = "ISO3", "Year"="Year"))
avgFixed <- avgFixed %>% dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI,FSI)

# Joining Metadata
avgFixed <- left_join(avgFixed, metadata_countries,  by =c("ISO3" = "ISO3") )

# Adding Growth Rate Column
avgFixed <- avgFixed %>%
  dplyr::mutate(YearPrev = Year-1)
avgFixed <- avgFixed %>%
  dplyr::mutate(YearNext = Year+1)

# Prepare Dataset to fix - Prev and Next Values
avgFixed <- left_join(avgFixed, itu_select,  by =c("ISO3" = "ISO3", "YearPrev"="Year"),suffix = c("", "Prev") )
avgFixed <- left_join(avgFixed, itu_select,  by =c("ISO3" = "ISO3", "YearNext"="Year"),suffix = c("", "Next") )

# Calculate Growth Rate (GR Columns)
avgFixed <- avgFixed %>%
  dplyr::mutate(FBSGR=(FBS-FBSPrev)/FBSPrev*100, FTSGR=(FTS-FTSPrev)/FTSPrev*100, MTSGR=(MTS-MTSPrev)/MTSPrev*100, FSIGR=(FSI-FSIPrev)/FSIPrev*100, PUIGR=PUI-PUIPrev,FSIGR=FSI-FSIPrev)

# Select Columns
avgFixed <- avgFixed %>%
  dplyr::select("Country","Year","ISO2","ISO3","UN","FBS", "FTS", "PUI", "MTS", "FSI", "economy","grp", "region", "subregion" ,"YearPrev" , "FBSPrev","FTSPrev","MTSPrev","PUIPrev","FSIPrev","FBSGR",  "FTSGR",  "MTSGR", "FSIGR",  "PUIGR")

# Rejoin Source Dataset
phase1.mod.itu <- union(delta.itu,avgFixed,by =c("ISO3" = "ISO3", "Year"="Year")) 

# Remove "not to consider" countries
phase1.mod.itu <- anti_join(phase1.mod.itu,aggNaNtc,by =c("ISO3" = "Group.1"))

# In the NA Fix Data 86 Rows are in the Year 2018 - Therefore we will use the avg. growth rate
# To fix the Data Rows of the 3 other countries in the 89 rows we will handle in Phase 3
# Afghanistan 2008 AF AFG 004 0.001803604 NA 1.840000 28.49300 2 
# Chad 2008 TD TCD 148 NA 0.40237534 1.190000 14.30668 3 
# Liberia 2017 LR LBR 430 0.191398712 NA 7.984480 56.56895 

phase2.mod.itu <- phase1.mod.itu %>%
  dplyr::select("Country","Year","ISO2","ISO3","UN","FBS", "FTS", "PUI", "MTS") %>% 
  filter(between(Year, 2008, 2018)) %>% 
  filter(is.na(FBS) | is.na(FTS) | is.na(PUI) | is.na(MTS))

# Phase 2
# AVG FIX select

avgFixGr <- phase2.mod.itu %>%
  dplyr::select("Country","Year","ISO2","ISO3","UN","FBS", "FTS", "PUI", "MTS") %>% 
  filter(between(Year, 2018, 2018)) %>% 
  filter_all(any_vars( is.na(.)))

avgFixGr <- avgFixGr %>%
  dplyr::mutate(YearPrev = Year-1)

# Determine Joing Dataset
avgFixGr_select <- phase1.mod.itu %>%
  dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI,FSI) %>%
  dplyr::filter(ISO3 %in% avgFixGr$ISO3)

# Join Prev Year Data
avgFixGr.in <- left_join(avgFixGr, avgFixGr_select,  by =c("ISO3" = "ISO3", "YearPrev"="Year"),suffix = c("", "Prev") )

# Prepare DS avg ref data
avgFixGr_select <- avgFixGr_select %>%
  dplyr::mutate(YearPrev = Year-1) %>%
  dplyr::mutate(YearNext = Year+1)

# Prepare Dataset to fix 
avgFixGr_select <- left_join(avgFixGr_select, avgFixGr_select,  by =c("ISO3" = "ISO3", "YearPrev"="Year"),suffix = c("", "Prev"), KEEP=TRUE )
avgFixGr_select <- left_join(avgFixGr_select, avgFixGr_select,  by =c("ISO3" = "ISO3", "YearNext"="Year"),suffix = c("", "Next"), KEEP=TRUE )

# Calculate Growth Rate (GR Columns)
avgFixGr_select <- avgFixGr_select %>%
  dplyr::mutate(FBSGR=(FBS-FBSPrev)/FBSPrev*100, FTSGR=(FTS-FTSPrev)/FTSPrev*100, MTSGR=(MTS-MTSPrev)/MTSPrev*100, FSIGR=(FSI-FSIPrev)/FSIPrev*100, PUIGR=PUI-PUIPrev,FSIGR=FSI-FSIPrev)

# Get average GR of the past 10 years
avgFixGr.ds <- avgFixGr_select %>%
  group_by(ISO3) %>% 
  filter(ISO3 %in% avgFixGr$ISO3) %>% 
  filter(between(Year, 2008, 2017)) %>% 
  summarize(FBSGRM = mean(FBSGR),FTSGRM = mean(FTSGR),MTSGRM = mean(MTSGR),PUIGRM = mean(PUIGR))

# Join PUI Rows to be fixed
avgFixGr.mod <- inner_join(avgFixGr.in,avgFixGr.ds ,by =c("ISO3" = "ISO3"))

# Add growth rate median to previouse PUI value
avgFixGr.modPUIFixed <- avgFixGr.mod %>%
  filter(is.na(PUI)) %>%
  dplyr::mutate(PUI= (PUIPrev+PUIGRM)/2 )

avgFixGr.modMTSFixed <- avgFixGr.mod %>%
  filter(is.na(MTS)) %>%
  dplyr::mutate(MTS = (MTSPrev+MTSGRM)/2) 

avgFixGr.modFTSFixed <- avgFixGr.mod %>%
  filter(is.na(FTS)) %>%
  dplyr::mutate(FTS= (FTSPrev+FTSGRM)/2 )

avgFixGr.modFBSFixed <- avgFixGr.mod %>%
  filter(is.na(FBS)) %>%
  dplyr::mutate(FBS= (FBSPrev+FBSGRM)/2 )

# For some countries like ETH this calculation method is not well suited, because of very big value ranges
# For this reason we had to change our calculation method / we decided to take the mean of the past decade to avoid further problems.
# After trying to work with the mean of the past 10 years and realzing it won't be any good, we came up with a new idea. 
# We will use datasets Country=World figures, calc the annual growth rates and multiply them with the previous year values.
# World / Year 2018 / Year 2017 = Multiplier
MFBS<-	14.06349744	/ 13.32110405 
MFTS<- 	12.49375919	/ 12.98777404 
MMTS<-	104.0133119	/ 103.6230792 
MPUI<-  51.2 / 48.6

avgFixGr.modPUIFixed <- avgFixGr.mod %>%
  filter(is.na(PUI)) %>%
  dplyr::mutate(PUI= (PUIPrev*MPUI))
avgFixGr.modMTSFixed <- avgFixGr.mod %>%
  filter(is.na(MTS)) %>%
  dplyr::mutate(MTS = (MTSPrev*MMTS) )
avgFixGr.modFTSFixed <- avgFixGr.mod %>%
  filter(is.na(FTS)) %>%
  dplyr::mutate(FTS= (FTSPrev * MFTS))
avgFixGr.modFBSFixed <- avgFixGr.mod %>%
  filter(is.na(FBS)) %>%
  dplyr::mutate(FBS= (FBSPrev *MFBS))

phase2.mod.itu <- avgFixGr.mod %>%
  mutate(FBS = ifelse(is.na(FBS), FBSPrev * MFBS, FBS),
         FTS = ifelse(is.na(FTS), FTSPrev * MFTS, FTS),
         MTS = ifelse(is.na(MTS), MTSPrev * MMTS, MTS),
         PUI = ifelse(is.na(PUI), PUIPrev * MPUI, PUI))

phase2.mod.itu  <- phase2.mod.itu  %>%
  dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI)

# Phase 3 
# Afghanistan 2008 AF AFG 004 0.001803604 NA 1.840000 28.49300 2 
# Chad 2008 TD TCD 148 NA 0.40237534 1.190000 14.30668 3 
# Liberia 2017 LR LBR 430 0.191398712 NA 7.984480 56.56895 
# For phase 3 we had to reverse the multiplier for two figures
# World FTS / Year 2008 / Year 2009 = Multiplier
MFTS2008 <-	18.5165507 / 18.35768431
# World FBS / Year 2008 / Year 2009 = Multiplier
MFBS2008 <-	6.090598277	/ 6.855867039
# World FTS / Year 2017 / Year 2016 = Multiplier
MFTS2017 <-	12.98777404 / 13.50933002

phase3.mod.itu <- phase1.mod.itu %>%
  dplyr::select("Country","Year","ISO2","ISO3","UN","FBS", "FTS", "PUI", "MTS") %>% 
  filter(between(Year, 2008, 2017)) %>% 
  filter(is.na(FBS) | is.na(FTS) | is.na(PUI) | is.na(MTS))

phase3.mod.itu <- phase3.mod.itu %>%
  dplyr::mutate(YearPrev = Year-1) %>%
  dplyr::mutate(YearNext = Year+1)
# Prepare Dataset to fix 
phase3.mod.itu <- left_join(phase3.mod.itu, itu_select,  by =c("ISO3" = "ISO3", "YearPrev"="Year"),suffix = c("", "Prev"), KEEP=TRUE )
phase3.mod.itu <- left_join(phase3.mod.itu, itu_select,  by =c("ISO3" = "ISO3", "YearNext"="Year"),suffix = c("", "Next"), KEEP=TRUE )

phase3.mod.itu <- phase3.mod.itu %>%
  mutate(FTS = ifelse(ISO3 =="AFG", FTSNext * MFTS2008, FTS)) %>%
  mutate(FBS = ifelse(ISO3 =="TCD", FBSNext * MFBS2008, FBS)) %>%
  mutate(FTS = ifelse(ISO3 =="LBR", FTSPrev * MFTS2017, FTS)) 

phase3.mod.itu  <- phase3.mod.itu  %>%
  dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI)

# Combine all phase and recalculated figures
phase4.mod.itu <- rbind(phase2.mod.itu,phase3.mod.itu)

phase5.mod.itu <- phase1.mod.itu %>%
  dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI) %>%
  filter(between(Year, 2008, 2018)) 

phaseF.mod.itu <- anti_join(phase5.mod.itu,phase4.mod.itu,by =c("ISO3" = "ISO3", "Year"="Year"))
phaseF.mod.itu <- rbind(phaseF.mod.itu,phase4.mod.itu)

# Find missing Rows per Year
aggMR.FBS <- phaseF.mod.itu %>% select(ISO3,Year,FBS)
aggMR.FBS <- aggMR.FBS %>%  pivot_wider(names_from = Year, values_from = FBS)
aggMR.FBS <- aggMR.FBS %>% filter_all(any_vars( is.na(.))) 
aggMR.FBS <- aggMR.FBS %>% pivot_longer(  cols = -ISO3,  names_to = "Year",  values_to = "FBS")
aggMR.FBS <- aggMR.FBS %>% filter_all(any_vars( is.na(.)))
aggMR.FTS <- phaseF.mod.itu %>% select(ISO3,Year,FTS)
aggMR.FTS <- aggMR.FTS %>%  pivot_wider(names_from = Year, values_from = FTS)
aggMR.FTS <- aggMR.FTS %>% filter_all(any_vars( is.na(.))) 
aggMR.FTS <- aggMR.FTS %>% pivot_longer(  cols = -ISO3,  names_to = "Year",  values_to = "FTS")
aggMR.FTS <- aggMR.FTS %>% filter_all(any_vars( is.na(.)))
aggMR.MTS <- phaseF.mod.itu %>% select(ISO3,Year,MTS)
aggMR.MTS <- aggMR.MTS %>%  pivot_wider(names_from = Year, values_from = MTS)
aggMR.MTS <- aggMR.MTS %>% filter_all(any_vars( is.na(.))) 
aggMR.MTS <- aggMR.MTS %>% pivot_longer(  cols = -ISO3,  names_to = "Year",  values_to = "MTS")
aggMR.MTS <- aggMR.MTS %>% filter_all(any_vars( is.na(.)))
aggMR.PUI <- phaseF.mod.itu %>% select(ISO3,Year,PUI)
aggMR.PUI <- aggMR.PUI %>%  pivot_wider(names_from = Year, values_from = PUI)
aggMR.PUI <- aggMR.PUI %>% filter_all(any_vars( is.na(.))) 
aggMR.PUI <- aggMR.PUI %>% pivot_longer(  cols = -ISO3,  names_to = "Year",  values_to = "PUI")
aggMR.PUI <- aggMR.PUI %>% filter_all(any_vars( is.na(.)))

# Join all missing Year Values
aggMR <- left_join(aggMR.FBS,aggMR.FTS,by =c("ISO3" = "ISO3", "Year"="Year"))
aggMR <- left_join(aggMR,aggMR.MTS,by =c("ISO3" = "ISO3", "Year"="Year"))
aggMR <- left_join(aggMR,aggMR.PUI,by =c("ISO3" = "ISO3", "Year"="Year"))

# Get prev. Year
aggMR <- aggMR %>%
  dplyr::mutate(YearPrev = as.integer(Year)-1)

# Prepare Dataset to fix 
aggMR <- left_join(aggMR, phaseF.mod.itu,  by =c("ISO3" = "ISO3", "YearPrev"="Year"),suffix = c("", "Prev"), KEEP=TRUE )

# Generate DS with defined multiplier
aggMR<- aggMR %>%
  filter(is.na(PUI)) %>%
  dplyr::mutate(PUI= (PUIPrev*MPUI))
aggMR <- aggMR %>%
  filter(is.na(MTS)) %>%
  dplyr::mutate(MTS = (MTSPrev*MMTS) )
aggMR <- aggMR %>%
  filter(is.na(FTS)) %>%
  dplyr::mutate(FTS= (FTSPrev * MFTS))
aggMR <- aggMR %>%
  filter(is.na(FBS)) %>%
  dplyr::mutate(FBS= (FBSPrev *MFBS))

aggMR  <- aggMR  %>%
  dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI) %>%
  dplyr::mutate(Year= as.integer(Year))

phaseF.mod.itu <- anti_join(phaseF.mod.itu,aggMR,by =c("ISO3" = "ISO3", "Year"="Year"))
phaseF.mod.itu <- rbind(phaseF.mod.itu,aggMR)
phaseF.mod.itu  <- phaseF.mod.itu  %>%
  dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI)

# Check for NA Values
phaseCNA.mod.itu <- phaseF.mod.itu %>%
  filter_all(any_vars( is.na(.)))

# Check for Row Count
phaseCRC.mod.itu <- dataset_itu_index %>%
  filter(between(Year, 2008, 2018)) %>%                   # Filter for last Decade
  filter((ISO3 %in% dataset_fsi_index$ISO3) == TRUE) %>%  # Remove Countries without FSI Index
  filter((ISO3 %in% aggNaNtc$Group.1) == FALSE)           # Remove Countries without sufficient data
                                                          # 1737 + 12 Rows from Countries without 2018 Entry
                                                          # 1749 -> Beautiful :)

countryname <- dataset_itu_index %>%
  dplyr::select(Country, ISO3) %>%
  group_by(Country, ISO3) %>% 
  summarize()

phaseclean.mod.itu <- inner_join(countryname,phaseF.mod.itu, by=c("ISO3" = "ISO3"))
xexport.itu <- left_join(phaseclean.mod.itu, dataset_fsi_index,by = c("ISO3" = "ISO3", "Year" = "Year"))
xexport.itu <- left_join(xexport.itu, metadata_countries, by =c("ISO3" = "ISO3") )

# Adding Growth Rate Column
xexport.itu <- xexport.itu %>% dplyr::mutate(YearPrev = Year-1)
# Calculate ITU Figure
xexport.itu <- xexport.itu %>% dplyr::mutate(ICT = (FTS / 60 * 0.2 * 0.4) + (MTS / 120 * 0.2 * 0.4) + (PUI / 100 * 0.33 * 0.4))

# Join Prev Year Data
xexport.itu <- left_join(xexport.itu, xexport.itu %>% dplyr::select(ISO3,Year,FBS,FTS,MTS,PUI,FSI,ICT),  by =c("ISO3" = "ISO3", "YearPrev"="Year"),suffix = c("", "Prev") )
xexport.itu <- xexport.itu %>% dplyr::mutate(FBSPrev= FBSPrev %>% replace_na(0)) %>% dplyr::mutate(FTSPrev= FTSPrev %>% replace_na(0)) %>% dplyr::mutate(MTSPrev= MTSPrev %>% replace_na(0)) %>% dplyr::mutate(PUIPrev= PUIPrev %>% replace_na(0)) %>% dplyr::mutate(FSIPrev= FSIPrev %>% replace_na(0)) %>% dplyr::mutate(ICTPrev= ICTPrev %>% replace_na(0)) 
xexport.itu = subset(xexport.itu, select = -c(`CountryPrev`) )
# Calculate Growth Rate (GR Columns)
xexport.itu <- xexport.itu %>% dplyr::mutate(FBSGR=(FBS-FBSPrev)/FBSPrev*100, FTSGR=(FTS-FTSPrev)/FTSPrev*100, MTSGR=(MTS-MTSPrev)/MTSPrev*100,PUIGR=PUI-PUIPrev,FSIGR=(FSI-FSIPrev)/FSIPrev*100,ICTGR=(ICT-ICTPrev)/ICTPrev*100)
xexport.itu <- xexport.itu %>% dplyr::mutate(FBSGR = ifelse(YearPrev==2007, 0, FBSGR)) %>% dplyr::mutate(FTSGR = ifelse(YearPrev==2007, 0, FTSGR)) %>% dplyr::mutate(MTSGR = ifelse(YearPrev==2007, 0, MTSGR)) %>% dplyr::mutate(PUIGR = ifelse(YearPrev==2007, 0, PUIGR)) %>% dplyr::mutate(FSIGR = ifelse(YearPrev==2007, 0, FSIGR))%>% dplyr::mutate(ICTGR = ifelse(YearPrev==2007, 0, ICTGR))
xexport.itu = subset(xexport.itu, select = -c(YearPrev,FBSPrev,FTSPrev,MTSPrev,PUIPrev,FSIPrev,ICTPrev) )
xexport.itu  <- xexport.itu  %>% mutate(STB = case_when(FSI < 70  ~ "stable",
                         FSI == 70  ~ "fragile",
                         FSI > 70  ~ "fragile",
                         is.na(FSI) ~ "!problem!"))

#Export 1749 Rows
write.csv(xexport.itu,xexport_ds_path, row.names = TRUE)



