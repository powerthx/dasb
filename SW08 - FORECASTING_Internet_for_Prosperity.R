library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggalt)
library(MASS)
library(ISLR)
library(tmap)

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
                                               "Year" = col_double(), 
                                               "ISO3" = col_character(), #FOCUS COUNTRIES "AFG","ALB","DZA","AGO","ATG","ARG","ARM","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BTN","BOL","BIH","BWA","BRA","BRN","BGR","BFA","BDI","KHM","CMR","CAN","CPV","CAF","TCD","CHL","CHN","COL","COM","COD","COG","CRI","CIV","HRV","CUB","CYP","CZE","PRK","DNK","DJI","DOM","ECU","EGY","SLV","GNQ","ERI","EST","ETH","FJI","FIN","FRA","GAB","GMB","GEO","DEU","GHA","GRC","GRD","GTM","GIN","GNB","GUY","HTI","HND","HUN","ISL","IND","IDN","IRN","IRQ","IRL","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KOR","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LTU","LUX","MDG","MWI","MYS","MDV","MLI","MLT","MRT","MUS","MEX","FSM","MDA","MNG","MNE","MAR","MOZ","MMR","NAM","NPL","NLD","NZL","NIC","NER","NGA","NOR","OMN","PAK","PAN","PNG","PRY","PER","PHL","POL","PRT","QAT","ROU","RUS","RWA","WSM","STP","SAU","SEN","SRB","SYC","SLE","SGP","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","SDN","SUR","SWZ","SWE","CHE","SYR","TJK","TZA","MKD","THA","TLS","TGO","TTO","TUN","TUR","TKM","UGA","UKR","ARE","GBR","USA","URY","UZB","VEN","VNM","YEM","ZMB","ZWE",
                                               "FBS" = col_double(), 
                                               "FTS" = col_double(), 
                                               "PUI" = col_double(), 
                                               "MTS" = col_double()))
# Remove aggregated Data Rows
dataset_itu_index <- dataset_itu_index %>%
  filter(`Country` %in% c("World","Developing","Asia & Pacific","Developed","The Americas","Europe","Africa","LDCs","Arab States","CIS","Ascension") == FALSE)

# Load FSI Index
dataset_fsi_index <- read_csv(dataset_fsi_path, 
                              col_types = cols(X1 = col_skip(),
                                               ISO3 = col_character(), 
                                               Total = col_double(), 
                                               Year = col_double()))
# Select relevant Columns from FSI Index
dataset_fsi_index <- dataset_fsi_index %>%
  dplyr::select("Year", "Total", "C1: Security Apparatus","C2: Factionalized Elites", "C3: Group Grievance", "E1: Economy", "E2: Economic Inequality","E3: Human Flight and Brain Drain","P1: State Legitimacy","P2: Public Services","P3: Human Rights", "S1: Demographic Pressures","S2: Refugees and IDPs","X1: External Intervention", "ISO3")

namesOfColumns <- c("Year","FSI",
                    "C1", #`C1: Security Apparatus`,
                    "C2", #`C2: Factionalized Elites`,
                    "C3", #`C3: Group Grievance`,
                    "E1", #`E1: Economy`,
                    "E2", #`E2: Economic Inequality`,
                    "E3", #`E3: Human Flight and Brain Drain`,
                    "P1", #`P1: State Legitimacy`,
                    "P2", #`P2: Public Services`,
                    "P3", #`P3: Human Rights`,
                    "S1", #`S1: Demographic Pressures`,
                    "S2", #`S2: Refugees and IDPs`,
                    "X1","ISO3")
colnames(dataset_fsi_index) <- namesOfColumns

# For EDA we only consider countires which are having a FSI Index
# This is sad because our data shows that a lot of countries like Monaco and Gibraltar have alot 
# of insight and stories to tell. Tax haven countries in particular look very well connected to the internet!
# Can we follow revenue streams and we will find internet coverage as well?!
itu <- inner_join(dataset_itu_index, dataset_fsi_index,by = c("ISO3" = "ISO3", "Year" = "Year"))

ds <- itu
names(ds)

# Remove unneeded columns
ds = subset(ds, select = c("FBS","FTS","PUI","MTS","FSI.x","C1", "C2", "C3", "E1", "E2", "E3","P1", "P2", "P3", "S1", "S2", "X1") )
ds <- ds %>% rename( FSI = FSI.x )

  datasetDecade <- ds
  dataset.corr <- cor(ds)
  
  palette = colorRampPalette(c("green", "white", "red")) (20)
  df = heatmap(x = dataset.corr, col = palette, symm = TRUE,Rowv = FALSE, Colv=FALSE)
  # Finding - Heatmap for Year 2018 only, on Attribtues, looks not good
  # Lets try to improved it with history over the past 10 years
  
  datasetDecade.corr <- cor(datasetDecade)
  
  palette = colorRampPalette(c("green", "white", "red")) (20)
  df = heatmap(x = datasetDecade.corr, col = palette, symm = TRUE,Rowv = FALSE, Colv=FALSE)
  
  #we concentrate on the "Fixed Broadband" as output for the regression
  correlation <- datasetDecade.corr[,'FBS']
  col <- colnames(datasetDecade.corr)  
  FBS.corr <- tibble(col, correlation)
  
  ggplot(data=FBS.corr) + geom_point(aes(x=col, y=correlation)) + geom_hline(yintercept=0, color="blue", size=2)
  
  #strating with the most correlated variable as predictor - S1: Demographic Pressures
  lm1 <- lm(FBS~S1,data=datasetDecade)
  dev.off()
  lm1
  summary(lm1)

  par(mfrow=c(2,2)); plot(lm1); par(mfrow=c(1,1))
  
  ggplot() + geom_point(data=datasetDecade, aes(x=S1, y=FBS)) + 
    geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="red", alpha=0.5) 
  
  #Let's do some predictions
  idx <- c(0.125, 1.2, 3, 6, 7.5, 8, 8.75, 9.5, 9.95)
  pred1 <- predict(lm1,data.frame(S1=idx), interval="prediction")
  pred2 <- predict(lm1,data.frame(S1=idx), interval="confidence")
  
  pred.frame <-  tibble(idx,pred1[,"fit"],pred1[,"lwr"],pred1[,"upr"],pred2[,"lwr"],pred2[,"upr"])
  colnames(pred.frame) <- c("x","y","y_min","y_max","conf_min","conf_max")
  
  ggplot(data=datasetDecade) +  geom_point(data=datasetDecade, aes(x=S1, y=FBS)) + 
    geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="grey", alpha=0.5) + 
    geom_smooth(data=datasetDecade, aes(x=S1, y=FBS)) +
    geom_point(data=pred.frame, aes(x=x, y=y),color="red", size=2) +
    geom_point(data=pred.frame, aes(x=x, y=y_min),color="red", size=4, alpha=0.5, shape =3) +
    geom_point(data=pred.frame, aes(x=x, y=y_max),color="red", size=4, alpha=0.5, shape =3) +
    geom_point(data=pred.frame, aes(x=x, y=conf_min),color="red", size=4, alpha=0.5, shape =95) +
    geom_point(data=pred.frame, aes(x=x, y=conf_max),color="red", size=4, alpha=0.5, shape =95)
  
  #let's have a look at the variable correlation with FBS, in graphical format
  ggplot(data=FBS.corr) + geom_point(aes(x=col, y=correlation)) + geom_hline(yintercept=0, color="blue", size=2)
  
  #second model, including also "P2"
  lm2 <- lm(FBS~S1-P2,data=datasetDecade)
  
  summary(lm2)
  
  #extending further, including also "E2"
  lm3 <- lm(FBS~S1-P2-E2,data=datasetDecade)
  
  summary(lm3)
  
  # what about using ALL the avaialble variables as predictor set (BAD IDEA, in general)
  lm.tot <- lm(FBS~.,data=datasetDecade)
  
  summary(lm.tot)
  
  #let's prune off the one not significant in the model 
  #  --> WE EXPECT this to be the BEST model possible, from the predictive point of view...
  lm.red <- lm(FBS~.-X1-P2-E3-C1,data=datasetDecade)
  
  summary(lm.red)
  
  #let's see what happens when we remove other predictors, starting from the less significant ones
  lm.red2 <- lm(FBS~.-C3-S2-X1-C1,data=datasetDecade)
  
  summary(lm.red2)
  
  #let's cpompare the models obtained, in a structured way
  anova(lm1, lm2, lm3, lm.red2, lm.red, lm.tot)

  
  ##############   Logistic   ##########################
  ds <- itu
  ds <- ds %>% rename( FSI = FSI.x )
  ds <- ds  %>%
    dplyr::select("FBS","FSI","STB")  

  summary(ds)
  View(ds)
  library(gridExtra) # for convenient window arrangement in plotting
  
  # Display the distributions of FSI and FBS
  p1 <- ggplot(ds, aes(x=FBS)) + geom_histogram(color="black", fill="white")
  p2 <- ggplot(ds, aes(x=FSI)) + geom_histogram(color="black", fill="white")
  grid.arrange(p1, p2, nrow = 1)
  par(mfrow=c(1,1))
  
  # Display the classes in the ds data
  p1 <- ggplot(ds, aes(x=FSI, y=FBS)) + geom_point(aes(col=STB))
  p2 <- ggplot(ds, aes(x=STB, y=FSI, fill=STB)) + geom_boxplot()
  p3 <- ggplot(ds, aes(x=STB, y=FBS, fill=STB)) + geom_boxplot()
  grid.arrange(p1, p2, p3, nrow = 1, widths = c(2, 1, 1))
  par(mfrow=c(1,1))
  
  
  # Create training and test data
  set.seed(213)
  indices <- sample(1:250, 250) # select 250 random samples
  test.data <- ds[indices,]
  table(test.data$STB)
  training.data <- ds[-indices,]
  table(training.data$STB)
  p1 <- ggplot() + geom_point(data = training.data, aes(x=FSI, y=STB), color='steelblue3') + 
    geom_point(data = test.data, aes(x=FSI, y=STB), color='darkred', size=4) 
  p2 <- ggplot() + geom_point(data = training.data, aes(x=FSI, y=FBS), color='steelblue3') + 
    geom_point(data = test.data, aes(x=FSI, y=FBS), color='darkred', size=4) 
  grid.arrange(p1, p2, nrow = 1)
  training.data <- training.data %>%
    mutate(STB = ifelse(STB == "stable",1,0 ))
  glm1 <- glm(STB~FSI, data=training.data, family="binomial")
  summary(glm1)
  
  # Making predictions
  
  # "Predicting" the TRAINING data 
  pred3 = predict(glm1) # No data set is supplied to the predict() function: the probabilities are computed 
  # for the training data that was used to fit the logistic regression model. 
  # --> Notice: Without the type option specified in predict we get the linear predictor scale (see next plot)
  pred3.df <- data.frame(FSI=training.data$FSI,prediction=pred3,STB=training.data$STB) 
  # make it a data frame for plotting purposes
  ggplot() + geom_point(data = pred3.df, aes(x=FSI, y=prediction, col=STB)) + 
    geom_hline(yintercept = 0) + geom_hline(yintercept = 1)
  
  pred.train.probs = predict(glm1, type = "response") 
  # With type = "response", we get the response variable scale, i.e., the probabilities.
  pred.train.probs.df <- data.frame(FSI=training.data$FSI,pred.train.probs=pred.train.probs,STB=training.data$STB) 
  # make it a data frame for plotting purposes
  ggplot() + geom_point(data = pred.train.probs.df, aes(x=FSI, y=pred.train.probs, col=STB)) + 
    geom_hline(yintercept = 0) + geom_hline(yintercept = 1) # Plot. 
  
  # Predicting the TEST data PROBABILITIES
  pred.test.probs <- predict(glm1, test.data, type = "response")
  pred.test.probs.df <- data.frame(FSI=test.data$FSI,pred.test.probs=pred.test.probs, STB=test.data$STB)
  # make it a data frame for plotting purposes
  ggplot() + geom_point(data = pred.test.probs.df, aes(x=FSI, y=pred.test.probs, col=STB), size=5) + 
    geom_hline(yintercept = 0) + geom_hline(yintercept = 1) + geom_hline(yintercept = 0.5, linetype="dashed") + ylim(0,1)
  
  # Predicting the TEST data CLASSES
  pred.test.classes <- rep("No",nrow(test.data)) 
  # In order to predict the classes, we must convert the predicted into class labels, Yes or No. We start by converting all to No.
  pred.test.classes[pred.test.probs > 0.5] = "Yes"  
  # Now we set those to Yes whose proobability is greater than 0.5.
  pred.test.classes.df <- data.frame(FSI=test.data$FSI,pred.test.classes=pred.test.classes)
  # make it a data frame for plotting purposes
  ggplot() + geom_point(data = pred.test.classes.df, aes(x=FSI, y=pred.test.classes, col=test.data$STB), size=5)
  
  # Confusion matrix  
  table(test.data$STB, pred.test.classes)
  
  # Calculating the validation error rate (percentage of incorrectly classified samples) as an estimate of the test error rate
  mean(pred.test.classes != test.data$STB)
  
  # Predicting probabilities and classes for a FSI of 1000 and 2000 Dollars:
  new.data <- data.frame(STB = c(TRUE, TRUE,FALSE,FALSE,FALSE), FSI= c(12, 20, 71, 75, 90), FBS=c(112, 110, 10,15, 9)) 
  # student and FBS are arbitrarily set, since they will not be used by predict
  predict(glm1, newdata = new.data, type = "response")
  
  
  # Logistic Regression with >1 PREDICTORS (including qualitative predictors)
  
  # Fitting the model to the training data 
  training.data <- training.data %>%
    mutate(STB = ifelse(STB == "stable",1,0 ))
  glm2 <- glm(STB~., family = "binomial", data = training.data) 
  summary(glm2)
  
  glm3 <- glm(STB~.-FBS, family = "binomial", data = training.data) 
  summary(glm3)

  anova(glm1, glm3, glm2, test = "Chisq")
  
  # Predicting probabilities and classes for a FSI of 1000 and 2000 Dollars:
  new.data <- data.frame(STB = c(FALSE,FALSE), FSI= c(77, 88), FBS=c(10, 5)) 
  (pred.glm2a <- predict(glm2, newdata = new.data, type = "response"))
  
  class.glm2a <- rep("No",length(pred.glm2a)) 
  class.glm2a[pred.glm2a > 0.5] = "Yes" 
  class.glm2a
  
  new.data2 <- data.frame(student = c("No", "Yes"), FSI= c(80, 20), FBS=c(8,10)) # student and FBS are arbitrarily set, since they will not be used by predict
  (pred.glm2b <- predict(glm2, newdata = new.data2, type = "response"))
  
  class.glm2b <- rep("No",length(pred.glm2b)) 
  class.glm2b[pred.glm2b > 0.5] = "Yes" 
  class.glm2b
  
  new.data3 <- data.frame(student = c("Yes", "Yes"), FSI= c(74, 90), FBS=c(30, 40)) # student and FBS are arbitrarily set, since they will not be used by predict
  (pred.glm2c <- predict(glm2, newdata = new.data3, type = "response"))
  
  class.glm2c <- rep("No",length(pred.glm2c)) 
  class.glm2c[pred.glm2c > 0.5] = "Yes" 
  class.glm2c
  
  
