library(dplyr)
library(readr)
library(tidyr)

# The list of almost valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "Fixed-broadband subscriptions",
                    "Fixed-telephone subscriptions" = "Fixed-telephone subscriptions",
                    "Individuals using the Internet" = "Individuals using the Internet" ,
                    "Mobile-cellular telephone subscriptions" = "Mobile-cellular telephone subscriptions",
                    "Active mobile-broadband subscriptions" = "Active mobile-broadband subscriptions"
)

dataset_itu_index <- read_csv("/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index.csv", 
                              col_types = cols(Country = col_character(), 
                                               Frequency = col_character(), ISO2 = col_character(), 
                                               ISO3 = col_character(), Indicator = col_character(), 
                                               `Measure Type` = col_character(), 
                                               `Measure Unit` = col_character(), 
                                               UN = col_character(), Value = col_double(), 
                                               X1 = col_skip(), Year = col_double()))

# Select dataset set in the project proposal 
selected_indicators <- dataset_itu_index %>%
    dplyr::select("Country","Year","Indicator","Measure Type","Measure Unit","Value","ISO2","ISO3","UN") %>%  # Remove Column , - Identical Data in "Measure Unit"
    filter(between(Year, 2008, 2018))  %>%
    filter(Indicator %in% indicators) %>%
    #filter(`Country` %in% c("World","Developing","Asia & Pacific","Developed","The Americas","Europe","Africa","LDCs","Arab States","CIS") == FALSE) %>%
    filter(`Measure Unit` %in% c("Per 100 inhabitants", "%"))  %>%
    filter(`Measure Type` %in% c("Per 100 inhabitants", "Percentage"))  %>%
    arrange(Country, Indicator, desc( Value) )

# This check should be empty but R interpretes the NA ISO Code of Namibia as NA Value - 43 Rows -> OK
Checking_for_NA_Values <- selected_indicators %>%
  filter_all(any_vars( is.na(.)))

# Pivot Dataset wide
dataset_itu_index_wide <- selected_indicators %>%
    pivot_wider(names_from = c(Year,`Measure Type`,`Measure Unit`,Indicator), values_from = Value)

# Export Dataset into CSV
write.csv(dataset_itu_index_wide,"/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index_wide.csv", row.names = TRUE)

# Pivot Dataset wide Year
dataset_itu_index_wide_year <- selected_indicators %>%
  pivot_wider(names_from = c(`Measure Type`,`Measure Unit`,Indicator), values_from = Value)

# Export Dataset into CSV
write.csv(dataset_itu_index_wide_year,"/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index_wide_year.csv", row.names = TRUE)

# Pivot Dataset on Indicators to produce value columns
dataset_itu_index_values = subset(dataset_itu_index, select = -c(`Measure Type`) )
dataset_itu_index_values <- selected_indicators %>%
  pivot_wider(names_from = c(`Measure Type`,`Measure Unit`,Indicator), values_from = Value)

# Export Dataset into CSV
namesOfColumns <- c("Country",
                    "Year",
                    "ISO2",
                    "ISO3",
                    "UN",
                    "FBS", #"Per 100 inhabitants_Per 100 inhabitants_Fixed-broadband subscriptions",
                    "FTS", #"Per 100 inhabitants_Per 100 inhabitants_Fixed-telephone subscriptions",
                    "PUI", #"Percentage_%_Individuals using the Internet",
                    "MTS", #"Per 100 inhabitants_Per 100 inhabitants_Mobile-cellular telephone subscriptions", 
                    "AMB", #`Per 100 inhabitants_Per 100 inhabitants_Active mobile-broadband subscriptions`,
                    "IUI"  #`Per 100 inhabitants_Per 100 inhabitants_Individuals using the internet`
                    )  
colnames(dataset_itu_index_values) <- namesOfColumns
write.csv(dataset_itu_index_values,"/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index_values.csv", row.names = TRUE)

