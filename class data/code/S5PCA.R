

#******************************************************
#
#					Session 5
#				
#				Principal Components
#
#******************************************************


#***********************************************************
#
#			Get the data (historical monthly total precipitation, 
#                   monthly average daily max temp,
#                   and monthly average daily min temp
#                   at Charlottesville and Richmond)
#
#***********************************************************

# Set working directory
sourcedir = "C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 5 files/R Code"
datadir = "C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/Lecture 5 files"

setwd(datadir)

# load monthly climate data for Charlottesville and Richmond
# data downloaded from http://www.sercc.com/climateinfo/historical/historical_va.html
# and partially cleaned
weather <- read.csv("VirginiaWeatherData.csv")

#********************************************************************
#
#		Visualize the data	
#
#********************************************************************
names(weather)

par(mfrow=c(2,3))
hist(weather$C_Precip,main='Charlottesville Precip')
hist(weather$C_Tmax,main='Charlottesville Max Temp')
hist(weather$C_Tmin,main='Charlottesville Min Temp')
hist(weather$R_Precip,main='Richmond Precip')
hist(weather$R_Tmax,main='Richmond Max Temp')
hist(weather$R_Tmin,main='Richmond Min Temp')

weather$C_Precip

# remove -999.00
noDataRows = union(union(union(union(union(which(weather$C_Precip == -999.00), which(weather$C_Tmax == -999.00)),
            which(weather$C_Tmin == -999.00)), which(weather$R_Precip == -999.00)),
            which(weather$R_Tmax == -999.00)), which(weather$R_Tmin == -999.00))

weather <- weather[-noDataRows,]

# how does it look now?
par(mfrow=c(2,3))
hist(weather$C_Precip,main='Charlottesville Precip')
hist(weather$C_Tmax,main='Charlottesville Max Temp')
hist(weather$C_Tmin,main='Charlottesville Min Temp')
hist(weather$R_Precip,main='Richmond Precip')
hist(weather$R_Tmax,main='Richmond Max Temp')
hist(weather$R_Tmin,main='Richmond Min Temp')


#********************************************************************
#
#		Principal Components with the Covariance vs. Correlation Matrix	
#
#********************************************************************
# Learn how to use `princomp` and `prcomp` functions
?princomp
?prcomp

# Principal Components with the Covariance and Correlation Matrices

pca_cov <- prcomp(weather)
pca_cor <- prcomp(weather, scale=T)

# View data in the first 2 PCs

par(mfrow=c(1,2))
biplot(pca_cov, main='Biplot with Covariance Matrix')
biplot(pca_cor, main='Biplot with Correlation Matrix')

# Variance plot

screeplot(pca_cov, main = "Scree Plot with Covariance Matrix")
screeplot(pca_cor, main = "Scree Plot with Correlation Matrix")

# Cumulative variance
setwd(sourcedir)
source("PCAplots.R")

cumplot(pca_cov, col = "blue")
cumplot(pca_cor, col = "blue")

# Loadings (called $rotation when using prcomp, $loadings when using princomp)

barplot(pca_cov$rotation[,1], main='PC1 Loadings with Covariance Matrix')
barplot(pca_cor$rotation[,1], main='PC1 Loadings with Correlation Matrix')

barplot(pca_cov$rotation[,2], main='PC2 Loadings with Covariance Matrix')
barplot(pca_cor$rotation[,2], main='PC2 Loadings with Correlation Matrix')

