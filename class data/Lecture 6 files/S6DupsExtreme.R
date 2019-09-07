
#******************************************************
#
#  				Session 4
#				Duplicates, 
#				Categorial Variable Relationships &
#				Extreme values
#
#******************************************************

##############
# Get the data
##############

# Source AccidentInput

source("C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 2 files/r files/AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data

acts <- file.inputl("C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 2 files/TrainData")

# Next a data frame with all accidents from all years from 2001 - 2017
# with columns that are consistent for all of these years

# Get a common set the variables

comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

# the combined data frame

totacts <- combine.data(acts, comvar)

dim(totacts)

#***********************************************************
#
#			Setup Categorical Variables
#
#***********************************************************


# Accident type
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))


# Type of train
totacts$TYPEQ <- as.numeric(totacts$TYPEQ)


# Now convert to factor
totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))


# Accident cause

totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor
totacts$Cause <- factor(totacts$Cause)

#***********************************************************
#
#			Extreme Points
#
#***********************************************************

hist(totacts$ACCDMG)

#Look also at TOTKLD and TOTINJ


# Get the values in the box plot

dmgbox <- boxplot(totacts$ACCDMG)

# extreme points

length(dmgbox$out)

# What proportion of accidents are extreme?

length(dmgbox$out)/nrow(totacts)

# Proportion of costs

sum(as.numeric(totacts$ACCDMG[which(totacts$ACCDMG > dmgbox$stats[5])]))/sum(as.numeric(totacts$ACCDMG))


# Create a data frame with just the extreme ACCDMG accidents

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

dim(xdmg)

# Look at the graphs of these extreme accidents

hist(xdmg$ACCDMG)
boxplot(xdmg$ACCDMG, col = "steelblue", main = "Accidents with Extreme Damage", ylab = "Cost ($)")

plot(1:17, tapply(xdmg$ACCDMG, xdmg$YEAR, sum), type = "l", xlab = "Year", ylab = "Total Damage ($)", main = "Total Accident Damage per Year")

# also plot number of accidents per year.

table(totacts$YEAR)
barplot(table(totacts$YEAR))

# Frequency of accident types

barplot(table(xdmg$TYPE)) #compare with the totacts plot

# SPM of metrics

# SPM of metrics & train variables

uva.pairs(xdmg[,c("ACCDMG", "TOTKLD", "TOTINJ", "TRNSPD")])

# Categorical variables

# Cause

bwplot(Cause~ ACCDMG, main = "Box Plots of Accident Damage by Cause", xlab = "Damage ($)", ylab = "Accident Cause", data = xdmg)

# Type of accident

#Extreme accidents
which(totacts$ACCDMG > dmgbox$stats[5])


#Use of jitter
bwplot(as.factor(YEAR)~jitter(ACCDMG, amount = 2.5e5), data = xdmg, main = "Box Plots of Extreme Accident Damage by Year (with jitter)", xlab = "Damage ($)", ylab = "Year")


par(mfrow = c(1,2))
boxplot(jitter(xdmg$ACCDMG, amount = 2.5e5), col = "steelblue", main = "Extreme Accident Damage with Jitter")
boxplot(xdmg$ACCDMG, col = "steelblue", main = "Extreme Accident Damage without Jitter")
par(mfrow = c(1,1))


# Conditioning on categorical variables

# on Cause
xyplot(ACCDMG~TRNSPD | Cause, main = "Extreme Damage vs. Train Speed Conditioned on Cause", xlab = "Train Speed", ylab = "Total Accident Damage", data = xdmg)

# on type of accident



# Repeat the above extreme point analysis but use TOTINJ + TOTKLD
# But wait until we do more cleaning

#***********************************************************
#
#			Heatmaps for categorical variabels
#
#***********************************************************

table(xdmg$Cause, xdmg$TYPE)

heatmap(table(xdmg$Cause, xdmg$TYPE), Rowv = NA, Colv = NA)

# With legend (optional)


install.packages("gplots", dependencies = T)

library(gplots)

heatmap.2(table(xdmg$Cause, xdmg$TYPE), Rowv = F, Colv = F)

source("http://www.phaget4.org/R/myImagePlot.R")

myImagePlot(table(xdmg$Cause, xdmg$TYPE), title = "No. of Accidents by Cause and Type of Accident")


#***********************************************************
#
#			Data Cleaning
#
#***********************************************************

# Let's look at the most extreme cost accidents. Are there any of particular interest?

which(xdmg$ACCDMG > 15e6)

# Duplicates?

# The max

which(xdmg$ACCDMG == max(xdmg$ACCDMG))

# Look at the narrative

as.matrix(names(xdmg))

xdmg[which(xdmg$ACCDMG == max(xdmg$ACCDMG)), 122:136]

# Are there other duplicates?

duplicated(xdmg[1:100, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])


# What about longitude and latitude?

# what about incident number?

which(xdmg$INCDTNO == "110058")

xdmg[which(xdmg$INCDTNO == "110058"),  c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]

duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

# Not duplicated

!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]))


#remove the duplicates

xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

dim(xdmg)
dim(xdmgnd)

# number of duplicates

nrow(xdmg) - nrow(xdmgnd)

