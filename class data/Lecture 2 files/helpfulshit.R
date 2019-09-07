library(lattice)

##load data
source("C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 2 files/r files/AccidentInput.R")

my.path <- "C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 2 files/TrainData"
setwd(my.path)

acts <- file.inputl(my.path)

totacts <- combine.data(acts)
##total data set


##new data frame using killed and injured together as a new metric
cas_zero <- totacts
cas_zero["Casualties"] <- NA
cas_zero$Casualties <- cas_zero$TOTINJ + cas_zero$TOTKLD

cas_NoDuplicates <- cas_zero[!(duplicated(cas_zero[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

cas_one <- cas_NoDuplicates[cas_NoDuplicates$Casualties > 0,]

##creating cause metric and applying values to them
cas_one$Cause <- rep(NA, nrow(cas_one))
cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "M")] <- "M"
cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "T")] <- "T"
cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "S")] <- "S"
cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "H")] <- "H"
cas_one$Cause[which(substr(cas_one$CAUSE, 1, 1) == "E")] <- "E"

##getting better labels for the type of accident and type of vehicle
cas_one$TYPE <- factor(cas_one$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))
cas_one$TYPEQ <- as.numeric(cas_one$TYPEQ)
cas_one$TYPEQ <- factor(cas_one$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))



##DOING THE SAME FOR ALL ACCIDENTS
cas_zero$Cause <- rep(NA, nrow(cas_zero))
cas_zero$Cause[which(substr(cas_one$CAUSE, 1, 1) == "M")] <- "M"
cas_zero$Cause[which(substr(cas_one$CAUSE, 1, 1) == "T")] <- "T"
cas_zero$Cause[which(substr(cas_one$CAUSE, 1, 1) == "S")] <- "S"
cas_zero$Cause[which(substr(cas_one$CAUSE, 1, 1) == "H")] <- "H"
cas_zero$Cause[which(substr(cas_one$CAUSE, 1, 1) == "E")] <- "E"

cas_zero$TYPE <- factor(cas_zero$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))
cas_zero$TYPEQ <- as.numeric(cas_zero$TYPEQ)
cas_zero$TYPEQ <- factor(cas_zero$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))

table(cas_zero$TOTINJ)

##set up of data with severity metrics >0
cas_NoDuplicates["TOTDMG"] <- NA
cas_NoDuplicates$TOTDMG <- cas_NoDuplicates$ACCDMG + cas_NoDuplicates$EQPDMG

dmgbox <- boxplot(cas_NoDuplicates$TOTDMG)
hzebox <-boxplot(cas_NoDuplicates$CARSHZD)
length(dmgbox$out)/nrow(cas_NoDuplicates)
length(hzebox$out)/nrow(cas_NoDuplicates)



bwplot(as.factor(YEAR)~TOTDMG, data = cas_NoDuplicates, main = "Box Plots of Accident Damage", xlab = "$damage", ylab = "Year")
bwplot(as.factor(YEAR)~CARSHZD, data = cas_NoDuplicates, main = "Box Plots of hazmat", xlab = "$damage", ylab = "Year")

severe_table <- cas_NoDuplicates[cas_NoDuplicates$TOTDMG > dmgbox$stats[5],]


bwplot(as.factor(YEAR)~TOTDMG, data = severe_table, main = "Box Plots of Total Damage", xlab = "$damage", ylab = "Year")


hzbox <-boxplot(severe_table$Casualties)
severe_table <- severe_table[severe_table$Casualties > hzbox$stats[5],]
severe_table <- severe_table[severe_table$CARSHZD > 0,]
bwplot(as.factor(YEAR)~TOTDMG, data = severe_table, main = "Box Plots of Total Damage", xlab = "$damage", ylab = "Year")





##The MULTIVARIATE ANALYSIS
severe_table$Cause <- rep(NA, nrow(severe_table))

severe_table$Cause[which(substr(severe_table$CAUSE, 1, 1) == "M")] <- "M"
severe_table$Cause[which(substr(severe_table$CAUSE, 1, 1) == "T")] <- "T"
severe_table$Cause[which(substr(severe_table$CAUSE, 1, 1) == "S")] <- "S"
severe_table$Cause[which(substr(severe_table$CAUSE, 1, 1) == "H")] <- "H"
severe_table$Cause[which(substr(severe_table$CAUSE, 1, 1) == "E")] <- "E"


severe_table$Cause <- factor(severe_table$Cause)
table(severe_table$Cause)
severe_table$type

plot(2001:2017, tapply(severe_table$TOTDMG, as.factor(severe_table$YEAR), sum), type = "l", ylab = "Damage ($)", xlab = "Year", main = "Total Damage per Year")

bwplot(Cause~ TOTDMG, main = "Box Plots of Log(Accident Damage)", xlab = "Damage ($)", ylab = "Accident Cause", data = severe_table)
bwplot(Cause~ log(TOTDMG+1), main = "Box Plots of Log(Accident Damage)", xlab = "log(Damage ($))", ylab = "Accident Cause", data = severe_table)

xyplot(TOTDMG~Casualties | Cause, main = "Damage vs. casualties Conditioned on Cause", xlab = "Total casualites", ylab = "Total Accident Damage", data = severe_table)


library(lattice)
xyplot(log(TOTDMG)~TRNSPD |TRNSPD, data = severe_table, type = c("p", "r"))
yplot(log(ACCDMG)~TRNSPD | Cause, data = severe_table, type = c("p", "r"))









##here is the project 2 code

##part 1

severe_table$Freight <- (severe_table$TYPEQ == 1)

severe_table$
severe_table$Type <- factor(severe_table$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))
severe_table$Derail <- (severe_table$Type == "Derailment")

severe_table$trdensty <- as.double(as.factor(severe_table$TRKDNSTY))
severe_table$trdensty <- as.double(severe_table$TRKDNSTY)

source("C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 2 files/r files/SPM_Panel.R")
uva.pairs(severe_table[,c("TRNSPD", "CARS", "TEMP", "HEADEND1")])

severe_table.lm1<-lm(ACCDMG~Derail+TONS+trdensty+Freight,data=severe_table)
summary(severe_table.lm1)

severe_table.lm1<-lm(ACCDMG~Derail+TONS+Freight,data=severe_table)
severe_table.lm2<-lm(ACCDMG~Derail+trdensty+Freight,data=severe_table)
severe_table.lm3<-lm(ACCDMG~Derail+TONS+trdensty,data=severe_table)
severe_table.lm4<-lm(ACCDMG~Derail+TONS,data=severe_table)
severe_table.lm5<-lm(ACCDMG~TONS+trdensty,data=severe_table)


oneModels <- c(1,2,3,4,5)
oneModels[1] <- severe_table.lm1
oneModels[2] <- severe_table.lm2
oneModels[3] <- severe_table.lm3
oneModels[4] <- severe_table.lm4
oneModels[5] <- severe_table.lm5

print(oneModels)





