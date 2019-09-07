#    			Session 7
#			
#	 Multiple Linear Regression 1
#
#******************************************************

library(lattice)

##load data
source("C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 2 files/r files/AccidentInput.R")

my.path <- "C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 2 files/TrainData"
setwd(my.path)

acts <- file.inputl(my.path)

totacts <- combine.data(acts)

##Build a data frame with only extreme accidents for ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]


total_casualties <-boxplot(totacts$TOTINJ + totacts$TOTKLD)
cas_zero <- totacts
cas_zero["Casualties"] <- NA
cas_zero$Casualties <- cas_zero$TOTINJ + cas_zero$TOTKLD

ca <- table(cas_zero$Casualties)
ca_data <- as.data.frame(ca)
ca_data


total_casualties <-boxplot(cas_zero$Casualties)

cas_zerod <- cas_zero[!(duplicated(cas_zero[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

total_casualtiesd <- boxplot(cas_zerod$Casualties)

cas_one <- cas_zerod[cas_zerod$Casualties > 0,]

## data in cas one is now a table of casualties across 1 years

cad <-table(cas_zerod$Casualties)
cad_data <- as.data.frame(cad)


##boxplots go ehre
bwplot(as.factor(YEAR)~Casualties, data = cas_one, main = "Box Plots of Accident Damage", xlab = "deaths", ylab = "Year")
    ##causes
cas_one$Cause <- rep(NA, nrow(cas_one))

cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "M")] <- "M"
cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "T")] <- "T"
cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "S")] <- "S"
cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "H")] <- "H"
cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "E")] <- "E"

cas_one$TYPE <- factor(cas_one$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))
bwplot(as.factor(TYPEQ)~Casualties, data = cas_one, main = "Box Plots of Accident Damage", xlab = "deaths", ylab = "Year")
table(cas_one$TYPE)
table(cas_one$YEAR)
table(cas_one$Cause)
sum(cas_one$Casualties)

cas_one$TYPEQ <- as.numeric(cas_one$TYPEQ)

cas_one[-4048, ]

# Now convert to factor- use actual categories from data dictionary to be more informative

cas_one$TYPEQ <- factor(cas_one$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))

## Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# Setup categorical variables
xdmgnd$Cause <- rep(NA, nrow(xdmgnd))

xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor

xdmgnd$Cause <- factor(xdmgnd$Cause)

xdmgnd$Type <- factor(xdmgnd$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

#***********************************************************
#  	Possible predictors of damage	
#***********************************************************

# SPM
# Principal components with the correlation matrix for extreme data with 1 metric and quantitative predictors.


source("C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 2 files/r files/SPM_Panel.R")
uva.pairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])


# PCA
#Scatter plot matricies for quantitative predictors and single metric.

source("C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 5 files/R code/PCAplots.R")
pred.pca <- princomp(cas_one[,c("Casualties", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )
biplot(pred.pca)


## Which predictors are most correlated with accident damage?


###############################
# Categorical plots

# heatmap
source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xdmgnd$Cause, xdmgnd$Type), title = "No. of Accidents by Cause and Type of Accident")

## Which accident causes and types have the highest numbers of extreme accidents?

# Type & TRNSPD
library(lattice)
xyplot(log(ACCDMG)~TRNSPD | Type, data = xdmgnd, type = c("p", "r"))


# Cause & TRNSPD
xyplot(log(ACCDMG)~TRNSPD | Cause, data = xdmgnd, type = c("p", "r"))


##What can you determine about the relationship between train speed and accident
##damages for different types / causes of accidents?

# Cause X Type and TRNSPD
xyplot(log(ACCDMG)~TRNSPD | Cause * Type, data = xdmgnd, type = c("p", "r"))


# Create the Derail variable & 
# then look at interactions with Cause
xdmgnd$Derail <- (xdmgnd$Type == "Derailment")

# plot xy with interactions of Derail and Cause
xyplot(log(ACCDMG)~TRNSPD | Cause * Derail, data = xdmgnd, type = c("p", "r"))


# Create a Freight variable
xdmgnd$Freight <- (xdmgnd$TYPEQ == 1)

# Interaction plots

# Interaction plots with quantitative variables
Speed <- cut(xdmgnd$TRNSPD, c(min(xdmgnd$TRNSPD),17,max(xdmgnd$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))

Cars <- cut(xdmgnd$CARS, c(min(xdmgnd$CARS),1,max(xdmgnd$CARS)), include.lowest = T, labels = c("low hzd", "high hzd"))


# Plot interaction between Speed and Cars
interaction.plot(Speed, Cars, log(xdmgnd$ACCDMG))


# Plot interaction between Freight and Speed
interaction.plot(xdmgnd$Freight, Speed, log(xdmgnd$ACCDMG))


# Plot interaction between Derailments and Speed
interaction.plot(xdmgnd$Derail, Speed, log(xdmgnd$ACCDMG))




## How do these results inform your hypotheses?
## Use the multivariate visualiztions as evidence to form at least 1 hypothesis.

####################################
#	Now repeat for TOTKLD + TOTINJ
####################################

####################################
# Linear Models
####################################

# Build linear regression models with different combinations of quantitative predictors to provide evidence for your hypothesis

# Single predictor
xdmgnd.lm1<-lm(ACCDMG~TEMP,data=xdmgnd)
summary(xdmgnd.lm1)
names(xdmgnd.lm1)
coef(xdmgnd.lm1)
sum(xdmgnd.lm1$res^2)


# Two predictors
xdmgnd.lm2<-lm(ACCDMG~TEMP+TRNSPD,data=xdmgnd)
summary(xdmgnd.lm2)
names(xdmgnd.lm2)
coef(xdmgnd.lm2)


#Three predictors
xdmgnd.lm3<-lm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd)
summary(xdmgnd.lm3)
coef(xdmgnd.lm3)



# 4. Interperet your model coefficients.  Do they make sense?



# 5. Interperet your developed models using the model utility test and t-test.



# 5a. Write out the null and alternative hypothesis for each of the tests.  



# 5b. Do you accept or reject H0 for each test?


# 4. Interperet your model coefficients.  Do they make sense?



# 5. Interperet your developed models using the model utility test and t-test.



# 5a. Write out the null and alternative hypothesis for each of the tests.  



# 5b. Do you accept or reject H0 for each test?
