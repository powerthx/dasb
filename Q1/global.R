library(tm)
library(wordcloud)
library(memoise)
library(readr)
library(tidyverse)
library(ggplot2)

# The list of valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "Fixed-broadband subscriptions",
               "Fixed-telephone subscriptions" = "Fixed-telephone subscriptions",
               #"Individuals using the Internet" = "Individuals using the Internet" ,
               "Mobile-cellular telephone subscriptions" = "Mobile-cellular telephone subscriptions"
)

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(indicator, year, freq) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(indicator %in% indicators))
    stop("Unknown Indicator")
  # Read ITU ICT Indicators into Tibble
  dataset_itu_index <- read_csv("/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index.csv", 
                                col_types = cols(Country = col_character(), 
                                                 Frequency = col_character(), ISO2 = col_character(), 
                                                 ISO3 = col_character(), Indicator = col_character(), 
                                                 `Measure Type` = col_character(), 
                                                 `Measure Unit` = col_character(), 
                                                 UN = col_character(), Value = col_double(), 
                                                 X1 = col_skip(), Year = col_double()))
  ict_indicators <- as.data.frame(dataset_itu_index)
  namesOfColumns <- c("Year","Frequency","Indicator","Country","Measure Type","Mesure Unit","Value","ISO2","ISO3","UN")
  colnames(ict_indicators) <- namesOfColumns
  summary(ict_indicators) # summary

  ict_indicators <- as_tibble(ict_indicators)

  ## Question --> How has internet connectivity (Per 100 inhabitants) developed over the past decade?
  da <- ict_indicators %>%
    dplyr::select("Country","Year","Indicator","Measure Type","Mesure Unit","Value") %>%
    filter(between(Year, year, year))  %>%
    filter(Indicator == indicator) %>%
    filter(between(`Value` , 0, freq))  %>%
    filter(`Country` %in% c("World","Developing","Asia & Pacific","Developed","The Americas","Europe","Africa","LDCs","Arab States","CIS") == FALSE) %>%
    filter(`Mesure Unit` %in% c("Per 100 inhabitants"))  %>%
    arrange(desc(Value) )
  m <- data.frame("words" = da$Country,"freq" = da$Value)
})
