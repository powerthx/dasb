library(tidyverse)
library(modelr)
library(dplyr)
# Paths to the Datasets
xexport_ds_path<- "/home/tarnold/R/1PrjFINAL/transformed/dataset_internet_for_prosperity.csv"
dataset_fsi_index_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_fsi_index.csv"
# The list of almost valid indicators
indicators <<- list("Fixed-broadband subscriptions" = "FBS",
                    "Fixed-telephone subscriptions" = "FTS",
                    "Mobile-cellular telephone subscriptions" = "MTS",
                    "Percentage of Individuals using the Internet" = "PUI",
                    "ICT Index by UN Data" = "ICT",
                    "Fragile State Index" = "FSI")
## Read itu data source
dataset_itu_index_source <- read_csv(xexport_ds_path,
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



get_data <- function(xYear) {
  input_data <- data.frame(
    xaYear = c(xYear)
  )

  dataset_itu_index <- dataset_itu_index_source %>%
    filter(Year==xYear)

  start <- dataset_itu_index %>%
    select(region,FBS,FTS,MTS,FSI,ICT,PUI,STB) %>%
    dplyr::arrange(region, ICT) %>%
    group_by(region,STB) %>%
    summarise(FBS=sum(FBS),FTS=sum(FTS),MTS=sum(MTS),FSI=sum(FSI),ICT=sum(FBS)+sum(FTS)+sum(MTS)+sum(PUI)) %>%
    dplyr::mutate(target = case_when(STB == "stable"  ~ "5",
                                     STB == "fragile"  ~ "6")) %>%
    mutate(source = case_when(region == "Europe"  ~ "0",
                              region == "Asia"  ~ "1",
                              region == "Americas"  ~ "2",
                              region == "Africa"  ~ "3",
                              region == "Oceania"  ~ "4",
                              is.na(region) ~ "!problem!"))

  start <- start %>%
    dplyr::select(region,STB,ICT,source,target) %>%
    dplyr::mutate(target = as.integer(target)) %>%
    dplyr::mutate(source = as.integer(source))

  source <- dataset_itu_index %>%
    select(region) %>%
    dplyr::arrange(region) %>%
    group_by(region) %>%
    summarise() %>%
    mutate(source = case_when(region == "Europe"  ~ "0",
                              region == "Asia"  ~ "1",
                              region == "Americas"  ~ "2",
                              region == "Africa"  ~ "3",
                              region == "Oceania"  ~ "4",
                              is.na(region) ~ "!problem!"))

  middle <- dataset_itu_index %>%
    select(region,STB,"FBS", "FTS", "MTS","PUI") %>%
    pivot_longer(
      cols = c("FBS", "FTS", "MTS","PUI"),
      names_to = "Indicator",
      values_to = "ICT"
    )

  middle <- middle %>%
    select(region,Indicator,STB,ICT) %>%
    dplyr::arrange(region) %>%
    group_by(region,Indicator,STB) %>%
    summarise(ICT=sum(ICT)) %>%
    mutate(source = case_when(STB == "stable"  ~ "5",
                              STB == "fragile"  ~ "6")) %>%
    dplyr::mutate(target = case_when(Indicator == "FBS"  ~ "7",
                                     Indicator == "FTS"  ~ "8",
                                     Indicator == "MTS"  ~ "9",
                                     Indicator == "PUI"  ~ "10"))

  middle <- middle %>%
    dplyr::select(region,STB,ICT,source,target) %>%
    dplyr::mutate(target = as.integer(target)) %>%
    dplyr::mutate(source = as.integer(source))

  network <- rbind(start, middle)

  result <- network

  # return the result
  return(result)
}

get_data2 <- function(xSubregion, xYear) {
  input_data <- data.frame(
    xaSubregion = c(xSubregion), xaYear = c(xYear)
  )

  dataset_itu_index2 <- dataset_itu_index_source  %>%
    filter(Year==xYear) %>%
    filter(subregion==xSubregion)#"xSubregion")
  # dataset_itu_index2 <- dataset_itu_index_source  %>%
  #   filter(Year==2018) %>%
  #   filter(subregion=="Western Africa")#"xSubregion")

  dataset_fsi_index <- read_csv(dataset_fsi_index_path,
                                col_types = cols(X1 = col_skip(),`C1: Security Apparatus` = col_double(),
                                                 `C2: Factionalized Elites` = col_double(),
                                                 `C3: Group Grievance` = col_double(),
                                                 `E1: Economy` = col_double(), `E2: Economic Inequality` = col_double(),
                                                 `E3: Human Flight and Brain Drain` = col_double(),
                                                 ISO2 = col_character(), ISO3 = col_character(),
                                                 `P1: State Legitimacy` = col_double(),
                                                 `P2: Public Services` = col_double(),
                                                 `P3: Human Rights` = col_double(),
                                                 Rank = col_double(), `S1: Demographic Pressures` = col_double(),
                                                 `S2: Refugees and IDPs` = col_double(),
                                                 Total = col_double(), UN = col_double(),
                                                 `X1: External Intervention` = col_double(),
                                                 Year = col_integer()
                                ))

  names(dataset_fsi_index) <- c("Country",
                                "Year",
                                "Rank",
                                "IN0_Total",
                                "IN1_C1:Security Apparatus" ,
                                "IN2_C2:Factionalized Elites" ,
                                "IN3_C3:Group Grievance" ,
                                "IN4_E1:Economic Decline" ,
                                "IN5_E2:Uneven Economic Development" ,
                                "IN6_E3:Human Flight and Brain Drain" ,
                                "IN7_P1:State Legitimacy" ,
                                "IN8_P2:Public Services" ,
                                "IN9_P3:Human Rights and Rule of Law" ,
                                "IN10_S1:Demographic Pressures" ,
                                "IN11_S2:Refugees and IDPs" ,
                                "IN12_X1:External Intervention",
                                "ISO2",
                                "ISO3","UN")
  dataset_itu_index2 <- dataset_itu_index2 %>%
    mutate(Year = as.integer(Year))
  ds <- inner_join(dataset_itu_index2, dataset_fsi_index, by = c("ISO3" = "ISO3", "Year" = "Year"))

  ds <- ds %>%
    mutate(COHESION=`IN1_C1:Security Apparatus` +`IN2_C2:Factionalized Elites` + `IN3_C3:Group Grievance`) %>%
    mutate(ECONOMIC=`IN4_E1:Economic Decline` + `IN5_E2:Uneven Economic Development` + `IN6_E3:Human Flight and Brain Drain`) %>%
    mutate(POLITICAL=`IN7_P1:State Legitimacy` + `IN8_P2:Public Services` + `IN9_P3:Human Rights and Rule of Law`) %>%
    mutate(SOCIAL=`IN10_S1:Demographic Pressures` + `IN11_S2:Refugees and IDPs` + `IN12_X1:External Intervention`)

  ds <- ds %>%
    mutate(source = rank(Country.y)-1)

  max1<- ds %>%
    select(source) %>%
    group_by() %>%
    summarise(max(source))
  names(max1) <- "id"

  ds <- ds %>%
    mutate(target1 = dense_rank(STB)+max1$id)

  middle <- ds %>%
    select(Country.y,region,subregion,STB,"FBS", "FTS", "MTS","PUI",COHESION,ECONOMIC,POLITICAL,SOCIAL,source,target1) %>%
    pivot_longer(
      cols = c("FBS", "FTS", "MTS","PUI",COHESION,ECONOMIC,POLITICAL,SOCIAL),
      names_to = "Indicator",
      values_to = "ICT"
    )

  asdf<- middle %>%
    select(target1) %>%
    group_by() %>%
    summarise(max(target1))
  names(asdf) <- "id"

  end <- middle %>%
    mutate(target2 = dense_rank(Indicator)+asdf$id)

  f1 <- end %>%
    select(source,target1,ICT)
  f2 <- end %>%
    select(source,target2,ICT)

  names(f1) <- c("source","target","value")
  names(f2) <- c("source","target","value")
  network <- rbind(f1, f2)
  result <- network

  n1 <- end %>%
    select(Country.y,source) %>% group_by(Country.y,source) %>% summarise()
  n2 <- end %>%
    select(STB,target1) %>% group_by(STB,target1) %>% summarise()
  n3 <- end %>%
    select(Indicator,target2)  %>% group_by(Indicator,target2) %>% summarise()

  names(n1) <- c("name","id")
  names(n2) <- c("name","id")
  names(n3) <- c("name","id")
  n4 <- rbind(n1, n2)
  names <- rbind(n4, n3)

  # return the result
  return(result)
}

get_names <- function(xSubregion, xYear) {
  input_data <- data.frame(
    xaSubregion = c(xSubregion), xaYear =c(xYear)
  )

#  dataset_itu_index2 <- dataset_itu_index_source  %>%
#    filter(Year==2018) %>%
#    filter(subregion=="Western Africa")#"xSubregion")

  dataset_itu_index2 <- dataset_itu_index_source  %>%
    filter(Year==xYear) %>%
    filter(subregion==xSubregion)#"xSubregion")

  dataset_fsi_index <- read_csv(dataset_fsi_index_path,
                                col_types = cols(X1 = col_skip(),`C1: Security Apparatus` = col_double(),
                                                 `C2: Factionalized Elites` = col_double(),
                                                 `C3: Group Grievance` = col_double(),
                                                 `E1: Economy` = col_double(), `E2: Economic Inequality` = col_double(),
                                                 `E3: Human Flight and Brain Drain` = col_double(),
                                                 ISO2 = col_character(), ISO3 = col_character(),
                                                 `P1: State Legitimacy` = col_double(),
                                                 `P2: Public Services` = col_double(),
                                                 `P3: Human Rights` = col_double(),
                                                 Rank = col_double(), `S1: Demographic Pressures` = col_double(),
                                                 `S2: Refugees and IDPs` = col_double(),
                                                 Total = col_double(), UN = col_double(),
                                                 `X1: External Intervention` = col_double(),
                                                 Year = col_integer()
                                ))

  names(dataset_fsi_index) <- c("Country",
                                "Year",
                                "Rank",
                                "IN0_Total",
                                "IN1_C1:Security Apparatus" ,
                                "IN2_C2:Factionalized Elites" ,
                                "IN3_C3:Group Grievance" ,
                                "IN4_E1:Economic Decline" ,
                                "IN5_E2:Uneven Economic Development" ,
                                "IN6_E3:Human Flight and Brain Drain" ,
                                "IN7_P1:State Legitimacy" ,
                                "IN8_P2:Public Services" ,
                                "IN9_P3:Human Rights and Rule of Law" ,
                                "IN10_S1:Demographic Pressures" ,
                                "IN11_S2:Refugees and IDPs" ,
                                "IN12_X1:External Intervention",
                                "ISO2",
                                "ISO3","UN")



  dataset_itu_index2 <- dataset_itu_index2 %>%
    mutate(Year = as.integer(Year))
  ds <- inner_join(dataset_itu_index2, dataset_fsi_index, by = c("ISO3" = "ISO3", "Year" = "Year"))

  ds <- ds %>%
    mutate(COHESION=`IN1_C1:Security Apparatus` +`IN2_C2:Factionalized Elites` + `IN3_C3:Group Grievance`) %>%
    mutate(ECONOMIC=`IN4_E1:Economic Decline` + `IN5_E2:Uneven Economic Development` + `IN6_E3:Human Flight and Brain Drain`) %>%
    mutate(POLITICAL=`IN7_P1:State Legitimacy` + `IN8_P2:Public Services` + `IN9_P3:Human Rights and Rule of Law`) %>%
    mutate(SOCIAL=`IN10_S1:Demographic Pressures` + `IN11_S2:Refugees and IDPs` + `IN12_X1:External Intervention`)

  ds <- ds %>%
    mutate(source = rank(Country.y)-1)

  max1<- ds %>%
    select(source) %>%
    group_by() %>%
    summarise(max(source))
  names(max1) <- "id"

  ds <- ds %>%
    mutate(target1 = dense_rank(STB)+max1$id)
  #mutate(target1 = dense_rank(STB)+max(source))

  middle <- ds %>%
    select(Country.y,region,subregion,STB,"FBS", "FTS", "MTS","PUI",COHESION,ECONOMIC,POLITICAL,SOCIAL,source,target1) %>%
    pivot_longer(
      cols = c("FBS", "FTS", "MTS","PUI",COHESION,ECONOMIC,POLITICAL,SOCIAL),
      names_to = "Indicator",
      values_to = "ICT"
    )

  asdf<- middle %>%
    select(target1) %>%
    group_by() %>%
    summarise(max(target1))
  names(asdf) <- "id"

  end <- middle %>%
    mutate(target2 = dense_rank(Indicator)+asdf$id)

  f1 <- end %>%
    select(source,target1,ICT)
  f2 <- end %>%
    select(target1,target2,ICT)

  names(f1) <- c("source","target","value")
  names(f2) <- c("source","target","value")
  network <- rbind(f1, f2)
  result <- network

  n1 <- end %>%
    select(Country.y,source) %>% group_by(Country.y,source) %>% summarise()
  n2 <- end %>%
    select(STB,target1) %>% group_by(STB,target1) %>% summarise()
  n3 <- end %>%
    select(Indicator,target2)  %>% group_by(Indicator,target2) %>% summarise()

  names(n1) <- c("name","id")
  names(n2) <- c("name","id")
  names(n3) <- c("name","id")
  n4 <- rbind(n1, n2)
  names <- rbind(n4, n3)

    result <- names
    # return the result
    return(result)
}
