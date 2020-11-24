library(tmap)    # for static and interactive maps
library(rnaturalearth)
library(sp)
library(readr)
library(tidyr)
library(dplyr)
library(data.table)

# The list of valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "Fixed-broadband subscriptions",
                    "Fixed-telephone subscriptions" = "Fixed-telephone subscriptions",
                    #"Individuals using the Internet" = "Individuals using the Internet" ,
                    "Mobile-cellular telephone subscriptions" = "Mobile-cellular telephone subscriptions"
)

# Load polygons for Countries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load Dataset 
dataset_itu_index <- read_csv("/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index_wide.csv", 
                              col_types = cols(Country = col_character(), 
                                               ISO2 = col_character(), 
                                               ISO3 = col_character(), 
                                               X1 = col_skip()))
dataset_itu_index <- as.data.frame(dataset_itu_index)

worldmap <- world
worldmap = left_join(worldmap, dataset_itu_index, by=c("iso_a3"="ISO3"))