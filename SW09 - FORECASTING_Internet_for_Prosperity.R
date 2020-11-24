library(fpp2)
library(dplyr)
rm(list=ls())

# Paths to the Datasets
xexport_ds_path<- "/home/tarnold/R/1PrjFINAL/transformed/dataset_internet_for_prosperity.csv"
dataset_fsi_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_fsi_index.csv"

# The list of almost valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "FBS",
                    "Fixed-telephone subscriptions" = "FTS",
                    "Mobile-cellular telephone subscriptions" = "MTS",
                    "Percentage of Individuals using the Internet" = "PUI",
                    "Fragile State Index" = "FSI")

## Read itu data source
dataset_itu_index <- read_csv(xexport_ds_path, 
                              col_types = cols(X1 = col_skip(),
                                               "Country" = col_character(), 
                                               "Year" = col_integer(),#col_date(format = "%Y"),
                                               "ISO3" = col_character(), #FOCUS COUNTRIES "AFG","ALB","DZA","AGO","ATG","ARG","ARM","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BTN","BOL","BIH","BWA","BRA","BRN","BGR","BFA","BDI","KHM","CMR","CAN","CPV","CAF","TCD","CHL","CHN","COL","COM","COD","COG","CRI","CIV","HRV","CUB","CYP","CZE","PRK","DNK","DJI","DOM","ECU","EGY","SLV","GNQ","ERI","EST","ETH","FJI","FIN","FRA","GAB","GMB","GEO","DEU","GHA","GRC","GRD","GTM","GIN","GNB","GUY","HTI","HND","HUN","ISL","IND","IDN","IRN","IRQ","IRL","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KOR","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LTU","LUX","MDG","MWI","MYS","MDV","MLI","MLT","MRT","MUS","MEX","FSM","MDA","MNG","MNE","MAR","MOZ","MMR","NAM","NPL","NLD","NZL","NIC","NER","NGA","NOR","OMN","PAK","PAN","PNG","PRY","PER","PHL","POL","PRT","QAT","ROU","RUS","RWA","WSM","STP","SAU","SEN","SRB","SYC","SLE","SGP","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","SDN","SUR","SWZ","SWE","CHE","SYR","TJK","TZA","MKD","THA","TLS","TGO","TTO","TUN","TUR","TKM","UGA","UKR","ARE","GBR","USA","URY","UZB","VEN","VNM","YEM","ZMB","ZWE",
                                               "FBS" = col_double(), 
                                               "FTS" = col_double(), 
                                               "PUI" = col_double(), 
                                               "MTS" = col_double()))
ds <- dataset_itu_index %>%
  #dplyr::filter(ISO3 =="CHE") %>%
  #dplyr::filter(region =="Africa") %>%
  dplyr::arrange(Year,by_group = FALSE)%>%
  dplyr::select("Year", "FSI")#, "FTS","MTS","PUI","ICT")


ds$FSI %>%
  ets(model="AAN", damped=FALSE, lambda=0) %>%
  forecast(h=50, biasadj=TRUE) %>%
  autoplot(ts.geom = 'point', shape = 3)


# Bounds
a <- 50
b <- 400
# Transform data and fit model
fit <- log((eggs-a)/(b-eggs)) %>%
  ets(model="AAN", damped=FALSE)
fc <- forecast(fit, h=50)
# Back-transform forecasts
fc[["mean"]] <- (b-a)*exp(fc[["mean"]]) /
  (1+exp(fc[["mean"]])) + a
fc[["lower"]] <- (b-a)*exp(fc[["lower"]]) /
  (1+exp(fc[["lower"]])) + a
fc[["upper"]] <- (b-a)*exp(fc[["upper"]]) /
  (1+exp(fc[["upper"]])) + a
fc[["x"]] <- eggs
# Plot result on original scale
autoplot(fc)


dx <- dataset_itu_index %>%
  #dplyr::filter(ISO3 =="CHE") %>%
  #dplyr::filter(region =="Africa") %>%
  dplyr::arrange(Year,by_group = FALSE)


GGally::ggpairs(as.data.frame(dx[,4:9]))
da <- dataset_itu_index 
library(ggplot2)
library(tsibble)
library(ggfortify)


qplot(FSI, ICT, data=as.data.frame(dx)) +
  ylab("ICT") + xlab("FSI")


y <- ts(c(ds$FBS),start = 2008,end = 2018)
autoplot(y, ts.geom = 'point', shape = 3)


ts %>%
  ets(model="AAN", damped=FALSE, lambda=0) %>%
  forecast(h=50, biasadj=TRUE) %>%
  autoplot()



View(eggs)

asd <- ts(y)
tseries <- ts(ds$FBS, start = c(2008, 1), frequency = 1)
print(tseries)
plot(tseries)

Y <- ts(ds)
usnim_ts = ts(ds[, 2], start = c(2008, 1), frequency = 1)


autoplot(usnim_ts) +
  ggtitle("test") +
  ylab("test")

fit<- snaive(itu) 
print(summary(fit))
checkresiduals(fit)


autoplot(itu) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")
