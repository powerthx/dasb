library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(data.table)

# Paths to the Datasets
xexport_ds_path<- "/home/tarnold/R/1PrjFINAL/transformed/dataset_internet_for_prosperity.csv"

# The list of almost valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "FBS",
                    "Fixed-telephone subscriptions" = "FTS",
                    "Mobile-cellular telephone subscriptions" = "MTS",
                    "Percentage of Individuals using the Internet" = "PUI",
                    "Fragile State Index" = "FSI")

dataset_internet_for_prosperity <- read_csv("R/1PrjFINAL/transformed/dataset_internet_for_prosperity.csv", 
                                            col_types = cols(Country = col_character(), 
                                                             FBS = col_double(), FBSGR = col_double(), 
                                                             FSI = col_double(), FSIGR = col_double(), 
                                                             FTS = col_double(), FTSGR = col_double(), 
                                                             ICT = col_double(), ICTGR = col_double(), 
                                                             ISO3 = col_factor(levels = c("AFG","ALB","DZA","AGO","ATG","ARG","ARM","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BTN","BOL","BIH","BWA","BRA","BRN","BGR","BFA","BDI","KHM","CMR","CAN","CPV","CAF","TCD","CHL","CHN","COL","COM","COD","COG","CRI","CIV","HRV","CUB","CYP","CZE","PRK","DNK","DJI","DOM","ECU","EGY","SLV","GNQ","ERI","EST","ETH","FJI","FIN","FRA","GAB","GMB","GEO","DEU","GHA","GRC","GRD","GTM","GIN","GNB","GUY","HTI","HND","HUN","ISL","IND","IDN","IRN","IRQ","IRL","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KOR","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LTU","LUX","MDG","MWI","MYS","MDV","MLI","MLT","MRT","MUS","MEX","FSM","MDA","MNG","MNE","MAR","MOZ","MMR","NAM","NPL","NLD","NZL","NIC","NER","NGA","NOR","OMN","PAK","PAN","PNG","PRY","PER","PHL","POL","PRT","QAT","ROU","RUS","RWA","WSM","STP","SAU","SEN","SRB","SYC","SLE","SGP","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","SDN","SUR","SWZ","SWE","CHE","SYR","TJK","TZA","MKD","THA","TLS","TGO","TTO","TUN","TUR","TKM","UGA","UKR","ARE","GBR","USA","URY","UZB","VEN","VNM","YEM","ZMB","ZWE")),
                                                             MTS = col_double(), 
                                                             MTSGR = col_double(), PUI = col_double(), 
                                                             PUIGR = col_double(), STB = col_character(), 
                                                             X1 = col_skip(), Year = col_double(), 
                                                             'economy' = col_factor(levels = c("1. Developed region: G7","2. Developed region: nonG7","3. Emerging region: BRIC","4. Emerging region: MIKT","5. Emerging region: G20","6. Developing region","7. Least developed region")),
                                                             'grp' = col_factor(levels = c("1. High income: OECD","2. High income: nonOECD","3. Upper middle income","4. Lower middle income","5. Low income")),
                                                             'region' = col_factor(levels = c("Africa","Americas","Antarctica","Asia","Europe","Oceania","Seven seas (open ocean)")), 
                                                             'subregion' = col_factor(levels = c("Antarctica","Australia and New Zealand","Caribbean","Central America","Central Asia","Eastern Africa","Eastern Asia","Eastern Europe","Melanesia","Micronesia","Middle Africa","Northern Africa","Northern America","Northern Europe","Polynesia","Seven seas (open ocean)","South America","South-Eastern Asia","Southern Africa","Southern Asia","Southern Europe","Western Africa","Western Asia","Western Europe"))))
itu <- dataset_internet_for_prosperity
# Add Total Score (Column TOT) for overall connectivity (Broadband Internet, Telefon, Mobile Phone)
itu <- as_tibble(itu)
itu <- mutate(itu, TOT = itu$FBS+itu$FTS+itu$MTS)

# Check for data quality
# Set analysing year (2008/2018)
itu <- itu %>%
  #filter(Year == 2008)
  #Lets focus first on recent measures
 filter(Year == 2018)
# itu = subset(itu, select = -c(`AMB`) )# itu = subset(itu, select = -c(`IUI`) )# itu = subset(itu, select = -c(`PUI`) )# itu = subset(itu, select = -c(`FBS`) )# itu = subset(itu, select = -c(`FTS`) )# v <- itu %>%#   filter_all(any_vars( is.na(.)))

# EDA
FBSPlot <- ggplot(data = itu, mapping = aes(x = FBS))
FBSPlot + geom_histogram()

FTSPlot <- ggplot(data = itu, mapping = aes(x = FTS))
FTSPlot + geom_histogram()

MTSPlot <- ggplot(data = itu, mapping = aes(x = MTS))
MTSPlot + geom_histogram()

ggplot(itu, aes(region, FBS, color = subregion)) + 
  geom_boxplot() + 
  ggtitle("FBSs by region Boxplot") + 
  xlab("region") + 
  ylab("FBSs")

ggplot(itu, aes(region, FTS, color = subregion)) + 
  geom_boxplot() + 
  ggtitle("FTSs by region Boxplot") + 
  xlab("region") + 
  ylab("FTSs")

ggplot(itu, aes(region, MTS, color = subregion)) + 
  geom_boxplot() + 
  ggtitle("MTSs by region Boxplot") + 
  xlab("region") + 
  ylab("MTSs")

ggplot(itu, aes(region, PUI, color = subregion)) + 
  geom_boxplot() + 
  ggtitle("PUIs by region Boxplot") + 
  xlab("region") + 
  ylab("PUIs")

plot <- ggplot(data = itu, mapping = aes(x = region))  
plot + geom_violin(mapping = aes(y = FBS))

plot <- ggplot(data = itu, mapping = aes(x =economy))  
plot + geom_violin(mapping = aes(y = FBS))

plot <- ggplot(data = itu, mapping = aes(x = grp))  
plot + geom_violin(mapping = aes(y = FBS))

plot <- ggplot(data = itu, mapping = aes(x = region))  
plot + geom_violin(mapping = aes(y = FBS))

plot <- ggplot(data = itu, mapping = aes(x = region))  
plot + geom_violin(mapping = aes(y = MTS))

plot <- ggplot(data = itu, mapping = aes(x =economy))  
plot + geom_violin(mapping = aes(y = MTS))

plot <- ggplot(data = itu, mapping = aes(x = grp))  
plot + geom_violin(mapping = aes(y = MTS))

ggplot(itu, aes(x=TOT, y=FBS, color=region)) + 
  geom_point() +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="FBS")

# Wow this is looks like a straight line
ggplot(itu, aes(x=TOT, y=MTS, color=region)) + 
  geom_point() +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="MTS")

ggplot(itu, aes(x=TOT, y=MTS, color=STB)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="MTS")

ggplot(itu, aes(x=grp, y=FBS, color=region)) + 
  geom_point() +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="FTS")

ggplot(itu, aes(FBS)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=region)) + 
  xlab("FBSs") + 
  ggtitle("FBSs by region")

ggplot(itu, aes(FTS)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=region)) + 
  xlab("FTSs") + 
  ggtitle("FTSs by region")

ggplot(itu, aes(MTS)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=region)) + 
  xlab("MTSs") + 
  ggtitle("MTSs by region")

ggplot(itu, aes(x=FSI, y=FBS, color=grp)) + 
  geom_bin2d() +
  facet_wrap(vars(economy)) +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple", "yellow", "white")) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50),
                     labels=c(0,10,20,30,40,50),
                     name="FBS")

ggplot(itu, aes(x=FSI, y=MTS, color=economy)) + 
  geom_bin2d() +
  facet_wrap(vars(region)) +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple", "yellow", "white")) +
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150,175,200,225,250,275),
                     labels=c(0,25,50,75,100,125,150,175,200,225,250,275),
                     name="MTS")

ggplot(itu, aes(x=FSI, y=FTS, color=region)) + 
  geom_bin2d() +
  facet_wrap(vars(economy)) +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple", "yellow", "white")) +
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="FTS")

#  Interesting :)
ggplot(itu, aes(x=FBS, y=FSI)) +
  geom_density2d() +
  facet_grid(vars()) +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple", "yellow", "white")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150,175),
                     labels=c(0,25,50,75,100,125,150,175),
                     name="FSI")

# Mobile looks quite different
ggplot(itu, aes(x=MTS, y=FSI)) +
  geom_density2d() +
  facet_grid(vars()) +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple", "yellow", "white")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150,175,200),
                     labels=c(0,25,50,75,100,125,150,175,200),
                     name="FSI")


ggplot(itu, aes(x=FSI, y=MTS,color=region)) +
  geom_density2d() +
  facet_grid(vars(region)) +
  scale_y_continuous(name="MTS")

ggplot(itu, aes(x=FBS, y=MTS,color=grp)) + 
  geom_point() +
  facet_grid(vars(region)) +
  scale_color_manual(values=c("green", "red", "blue", "orange", "purple", "yellow", "white")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100,125,150,175,200,225),
                     labels=c(0,25,50,75,100,125,150,175,200,225),
                     name="MTS")

# it is not that easy / but wait, have a look at 2008 :|
ggplot(itu, aes(x=FBS, y=MTS,color=STB)) + 
geom_jitter() +
facet_grid(vars(region)) +
  scale_y_continuous(name="MTS")

ggplot(itu, aes(x=FBS, y=MTS,color=economy)) + 
  geom_point() +
  facet_grid(vars(region)) +
  scale_y_continuous(name="MTS")

ggplot(itu, aes(FSI, FBS)) +
geom_point() +
  facet_grid(vars(region)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(name="FBS")

ggplot(itu, aes(FBS, MTS)) +
  geom_point() +
  facet_grid(vars(STB)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(name="MTS")

### Hypothesis 1 ###
dt.merge <- itu
# Sum up ICT access over the last 10 years to evaluate the best and worst country
dt.sum.ICT <- aggregate(dt.merge$ICT,
                               by = list(Country = dt.merge$Country),
                               FUN=sum)

# Create DataTable with the best and the worst Country
dt.plot.ICT <- dt.merge %>%
  filter(Country == "United Arab Emirates" | Country == "Eritrea")

#plot the best and the worst Country based on the ICT
# g1 <- ggplot(dt.plot.ICT, mapping = aes(Year, ICT, group = Country)) +
#   geom_line(mapping = aes(color = Country)) +
#   labs(x = "Year", y = "ICT access",
#        title = "Hypothesis 1")
# 
# print(g1)


#plot the best and the worst Country based on the ICT
ggplot(dt.merge, mapping = aes(Year, ICT)) +
  geom_point(alpha = 0.1) +
  geom_line(dt.plot.ICT,
            mapping = aes(Year,
                          ICT,
                          group = Country,
                          color = Country),
            size = 1) +
  geom_boxplot(mapping = aes(Year,
                             ICT,
                             group = Year)) +
  labs(x = "Year", y = "ICT access",
       title = "Hypothesis 1")

### Hypothesis 2 ###

#Build the datatable with Year and mean-value
dt.Years.meanICT <- aggregate(dt.merge$ICT,
                              by = list(Year = dt.merge$Year),
                              FUN=mean,
                              na.rm=TRUE,
                              na.action=NULL)
setnames(dt.Years.meanICT, c("x"), c("mean_ICT"))

#Build the datatable with Year and median-value
dt.Years.medianICT <- aggregate(dt.merge$ICT,
                                by = list(Year = dt.merge$Year),
                                FUN=median,
                                na.rm=TRUE,
                                na.action=NULL)
setnames(dt.Years.medianICT, c("x"), c("median_ICT"))

# merge the two datatable
dt.Years.ICTaccess <- merge(dt.Years.meanICT, dt.Years.medianICT, by = "Year", all = T)

#plot the development of the internet connectivity
 g2 <- ggplot(dt.Years.sumICT, mapping = aes(Year, mean_ICT)) +
   geom_point() +
   geom_line(aes(group = 1)) +
   labs(x = "Year", y = "ICT access (mean)",
        title = "Hypothesis 2")

ggplot(dt.merge, mapping = aes(Year, ICT)) +
  geom_point(alpha = 0.1) +
  geom_line(dt.Years.ICTaccess,
            mapping = aes(Year, mean_ICT, group = 1, color = "Mean"), size = 1) +
  geom_line(dt.Years.ICTaccess,
            mapping = aes(Year, median_ICT, group = 1, color = "Median"), size = 1) +
  labs(x = "Year", y = "ICT access",
       title = "Hypothesis 2")


