
#*****************************
#
# Load air quality data and relevant libraries
#
#*****************************
datadir <- "C:/Users/ramara/Documents/Class notes/Systems master/SYS4021/class data/air quality data"
# install.packages(mtsdi)
setwd(datadir)
data <- read.table('AirQualityUCI.csv',header=T,sep=',')
setwd(sourcedir)

library(forecast)
library(tseries)
library(mtsdi)
library(MTS)
library(MASS)

summary(data)

install.packages("MTS")


#*****************************
#
# Impute data
#
#*****************************

setwd(datadir)

airquality = read.csv('AirQualityUCI.csv')

# replace -200 with NA
airquality[airquality == -200] <- NA

# convert integer type to numeric
intcols = c(4,5,7,8,9,10,11,12)
for(i in 1:length(intcols)){
  airquality[,intcols[i]] <- as.numeric(airquality[,intcols[i]])
}


# create new data frame with just CO, C6H6 and NO2
AQdata = airquality[,c(3,6,10)]

# impute missing air quality data
f <- ~ CO.GT. + C6H6.GT. + NO2.GT.
t <- c(seq(1,dim(AQdata)[1],1))
i <- mnimput(f, AQdata, eps=1e-3, ts=TRUE, method='gam', ga.control=list(formula=paste(names(AQdata)[c(1:3)],'~ns(t,2)')))

# set airquality to imputed data
AQdata <- i$filled.dataset

# aggregate to daily maxima for model building and rename variables for interpretability
dailyAQ <- aggregate(AQdata, by=list(as.Date(airquality[,1],"%m/%d/%Y")), FUN=max)
colnames(dailyAQ)[1] <- "Date"
colnames(dailyAQ)[2] <- "CO"
colnames(dailyAQ)[3] <- "C6H6"
colnames(dailyAQ)[4] <- "NO2"
summary(dailyAQ)

#*****************************
#
# Part 1: Building Univariate Time Series Models
#
#*****************************

# Build univariate time series models for each pollutant
co.ts <- ts(dailyAQ$CO)
c6h6.ts <- ts(dailyAQ$C6H6)
no2.ts <- ts(dailyAQ$NO2)

# Plots of each time series for pollutants
plot(co.ts)
plot(c6h6.ts)
plot(no2.ts)

#ACFs-- clear evidence of seasonality, as well as linear decay in the no2 time series. Suggests regressing on time is necessary
acf(co.ts)
acf(c6h6.ts)
acf(no2.ts)

#PACF-- again, evidence of seasonality
pacf(co.ts)
pacf(c6h6.ts)
pacf(no2.ts)

# Side by side comparison of ACFs and PACFs
par(mfrow=c(1,2))
acf(co.ts)
pacf(co.ts)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
acf(c6h6.ts)
pacf(c6h6.ts)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
acf(no2.ts)
pacf(no2.ts)
par(mfrow=c(1,1))

#*****************************
#
# Part 1a: Modeling trends
#
#*****************************

#Model trends
time.co<-c(1:(length(co.ts)))
time.c6h6 <- c(1:(length(c6h6.ts)))
time.no2 <- c(1:(length(no2.ts)))

co.trend<-lm(co.ts~time.co)
c6h6.trend<-lm(c6h6.ts~time.c6h6)
no2.trend<-lm(no2.ts~time.no2)

summary(co.trend)
summary(c6h6.trend)
summary(no2.trend)

# Trend is only significant at the 0.05 level for NO2, as hypothesized based on the time series plots from earlier

# Plot trendlines on top of time series plot
plot(co.ts)
abline(co.trend,col='red')

plot(c6h6.ts)
abline(c6h6.trend,col='red')

plot(no2.ts)
abline(no2.trend,col='red')

#*****************************
#
# Part 1b: Modeling Seasonal and Cyclical Components
#
#*****************************

# Seasonality: CO

# Get the periodogram for co.ts
pg.co <- spec.pgram(co.ts,spans=9,demean=T,log='no')

# Find the peak, max.omega.co
max.omega.co<-pg.co$freq[which(pg.co$spec==max(pg.co$spec))]

# Where is the peak?
max.omega.co

# This peak is not the relevant local maximum, so instead we will look at a list of maxima

sorted.omegas <- sort(pg.co$spec, decreasing=T, index.return=T)
sorted.Ts <- 1/pg.co$freq[sorted.omegas$ix]
sorted.Ts[1:20]

#Based on this list, we can identify a seasonal component of ~7 days

# Create day variable

co.day <- time.co %% 7
co.day <-as.factor(time.co %% 7) 

#March 10, 2004, Wednesday is first day of time series

Day <- rep(NA, length(co.ts))
Day[which((time.co %% 7)    == 1)] <- "W"  
Day[which((time.co %% 7)    == 2)] <- "Th"
Day[which((time.co %% 7)    == 3)] <- "F"
Day[which((time.co %% 7)    == 4)] <- "Sa"
Day[which((time.co %% 7)    == 5)] <- "Su"
Day[which((time.co %% 7)    == 6)] <- "M"
Day[which((time.co %% 7)    == 0)] <- "Tu"
Day <- as.factor(Day)

#Model CO with dummy variables for day
co.trendseason<-lm(co.ts~time.co+ Day)
summary(co.trendseason)
#Model CO with day variable, time and trigonometric terms for cyclical component
co.trendseason2<-lm(co.ts~time.co+ Day+cos(2*pi*time.co/365)+sin(2*pi*time.co/365))
summary(co.trendseason2) 
#Create arima model for co time series
co.arima <- auto.arima(co.ts)
summary(co.arima)
#Compare models
AIC(co.trendseason)
AIC(co.arima)
AIC(co.trendseason2)

#*****************************
# Seasonality: C6H6
#*****************************

# Get the periodogram for precip.ts
pg.c6h6 <- spec.pgram(c6h6.ts,spans=9,demean=T,log='no')

# Find the peak, max.omega.precip
max.omega.c6h6<-pg.c6h6$freq[which(pg.c6h6$spec==max(pg.c6h6$spec))]

# Where is the peak?
max.omega.c6h6

# This peak is not the relevant local maximum, so instead we will look at a list of maxima

sorted.omegas.c6h6 <- sort(pg.c6h6$spec, decreasing=T, index.return=T)
sorted.Ts.c6h6 <- 1/pg.c6h6$freq[sorted.omegas$ix]
sorted.Ts.c6h6[1:20]

#Based on this list, there does appear to be seasonality with a ~7 day period



#Models
c6h6.trendseason<-lm(c6h6.ts~time.c6h6+ Day)
summary(c6h6.trendseason)

c6h6.trendseason2<-lm(c6h6.ts~time.c6h6+ Day+cos(2*pi*time.c6h6/365)+sin(2*pi*time.c6h6/365))
summary(c6h6.trendseason2)

c6h6.arima <- auto.arima(c6h6.ts)
summary(c6h6.arima)

#Compare models
AIC(c6h6.trendseason)
AIC(c6h6.arima)
AIC(c6h6.trendseason2)

#*****************************
# Seasonality:NO2
#*****************************

# Get the periodogram for precip.ts
pg.no2 <- spec.pgram(no2.ts,spans=9,demean=T,log='no')

# Find the peak, max.omega.precip
max.omega.no2<-pg.no2$freq[which(pg.no2$spec==max(pg.no2$spec))]

# Where is the peak?
max.omega.no2

# This peak is not the relevant local maximum, so instead we will look at a list of maxima

sorted.omegas.no2 <- sort(pg.no2$spec, decreasing=T, index.return=T)
sorted.Ts.no2 <- 1/pg.no2$freq[sorted.omegas$ix]
sorted.Ts.no2[1:20]

# Again, we have a period of ~7 days so we will use day dummy variables


#Models for NO2
no2.trendseason<-lm(no2.ts~time.no2+ Day)
summary(no2.trendseason)

no2.trendseason2<-lm(no2.ts~time.no2+ Day+cos(2*pi*time.no2/365)+sin(2*pi*time.no2/365))
summary(no2.trendseason2)
no2.arima <- auto.arima(no2.ts)
summary(no2.arima)

#Compare models
AIC(no2.trendseason)
AIC(no2.arima)
AIC(no2.trendseason2)

# Auto arima on residuals of model

##Diagnostic plots

tsdiag(co.arima)
tsdiag(c6h6.arima)
tsdiag(no2.arima)

# Look at ACF and PACf and say it will likely be an AR(1) or whatever, but then use auto arima

#Residuals
plot(co.trendseason2.transformed.trig$residuals)
plot(c6h6.trendseason2.transformed.trig$residuals^(1/L2))
e.co.lm <- auto.arima(co.trendseason2.transformed.trig$residuals^(1/L1))
e.c6h6.lm <- auto.arima(c6h6.trendseason2.transformed.trig$residuals^(1/L2))
e.no2.lm <- auto.arima(no2.trendseason2.transformed.trig$residuals^(1/L3))

summary(e.co.lm)
summary(e.c6h6.lm)
summary(e.no2.lm)
summary(no2.arima)

#Are residuals correlated?
allResiduals <- data.frame(co.trendseason2$residuals, c6h6.trendseason2$residuals, no2.trendseason2$residuals)
colnames(allResiduals) <- c("co","c6h6","no2")
cor(allResiduals)

# simulate 1 year
e.co.sim <- arima.sim(n=365, list(ar=c(-.6138), ma=c(0, 0))) 
e.c6h6.sim <- arima.sim(n=365, list(ar=c(.1977,0.0646,0.005), ma=c(0,0))) -.1547
e.no2.sim <- arima.sim(n=365, list(ar=c(.3710), ma=c(-.7793, -.1182)))


# see if the simulated residuals are correlated
allSimulations <- data.frame(e.co.sim, e.c6h6.sim, e.no2.sim)
colnames(allSimulations) <- c("co","c6h6","no2")
cor(allSimulations)
#cor(dailyAQ$CO,dailyAQ$C6H6,dailyAQ$NO2)

# add mean predictions and plot simulation of Tmin and Tmax
next.yr.time <- c(1:(391))
next.yr.co <- data.frame(time.co = next.yr.time)
next.yr.c6h6 <- data.frame(time.co = next.yr.time)
next.yr.no2 <- data.frame(time.co = next.yr.time)


co.mean <- (predict(co.trendseason2.transformed.trig, newdata=next.yr.co))^(1/L1)
c6h6.mean <- (predict(c6h6.trendseason2.transformed.trig, newdata=next.yr.c6h6))^(1/L2)
no2.mean <- predict(no2.trendseason2.transformed.trig, newdata=next.yr.no2)^(1/L3)
co.mean <- co.mean[1:365]
c6h6.mean <- c6h6.mean[1:365]
no2.mean <- no2.mean[1:365]

co.sim <-  e.co.sim + co.mean
c6h6.sim <- e.c6h6.sim + (c6h6.mean)
no2.sim <- e.no2.sim + no2.mean



plot(co.sim, type='l', col='black')
lines(co.ts, col='red')
plot(c6h6.sim, col='black')
lines(c6h6.ts,col='red')
plot(no2.sim, col='black')
lines(no2.ts, col='red')

plot(c6h6.ts^L2)


#ACF and PACF of residuals
acf(co.trendseason2$residuals)
acf(c6h6.trendseason2$residuals)
acf(no2.trendseason2$residuals)

pacf(co.trendseason2$residuals)
pacf(c6h6.trendseason2$residuals)
pacf(no2.trendseason2$residuals)

#Observed cross correlation?

#### Part 2

#Is this enough for part 2? Is it fine to just build the VARMA model on the residuals?

# build VARMA model using residuals from part 1
AICmatrix <- matrix(NA, 4, 5)
for(p in 1:4){
  for(q in 0:4){
    varma.model <- VARMACpp(allResiduals, p=p, q=q, include.mean=F)
    AICmatrix[p,q+1] <- varma.model$aic
  }
}

# pick the model with the lowest AIC
AICmatrix

varma.model <- VARMACpp(allResiduals, p=1, q=2, include.mean=F)

# simulate year of data from VARMA model
varma.sim = VARMAsim(365,phi=varma.model$Phi,theta=varma.model$Theta,sigma=varma.model$Sigma)

# Compare correlation of simulated residuals to actual residuals
cor(varma.sim$series)
cor(allResiduals)

summary(varma.model)
varma.sim$series

# add mean predictions and plot simulation of Tmin and Tmax
plot(varma.sim$series[,1] + co.mean, type='l', col='black')
lines(co.ts, col='red')
plot(varma.sim$series[,2] + c6h6.mean,type='l', col='black')
lines(c6h6.ts, col='red')
plot(varma.sim$series[,3] + no2.mean,type='l', col='black')
lines(no2.ts, col='red')

###################### Bonus


#This has not been completed yet. The code below is copied in from code uploaded on collab for a different application.


# Forecasting:

# Compare precip.auto with precip.temp.lm on forecasting the next 6 months
# Start with precip.auto
precip.auto.forecast <- forecast(precip.auto, h=6)


# Prediction performance
# Create test set from precip data set with last 6 months
next.6mo.time <- c((length(precip.ts)-5):(length(precip.ts)))
next.6mo <- data.frame(time.precip = next.6mo.time, precip=precip.ts[next.6mo.time])
next.6mo.ts <- temp.ts[next.6mo.time]
next.6mo.ts <- ts(next.6mo$precip)

# Prediction for the next 6 months by precip.lm and precip.auto:
E_Y.pred <- predict(precip.lm, newdata=next.6mo)^(1/L)
e_t.pred <- forecast(precip.auto, h=6)
next.6mo.prediction <- E_Y.pred + e_t.pred$mean

# MSE:
mean((next.6mo.prediction-next.6mo$precip)^2)

# Plot actual values and predicted values and confidence intervals
plot(ts(next.6mo$precip),type='o',ylim=c(1,5))
lines(ts(next.6mo.prediction),col='red',type='o')
lines(1:6, E_Y.pred + e_t.pred$lower[,2], col = "red", lty = "dashed")
lines(1:6, E_Y.pred + e_t.pred$upper[,2], col = "red", lty = "dashed")
legend(1,4, legend = c("Actual", "Predicted"), lwd = 2, col = c("black", "red")) 

# Repeat with precip.temp.lm
# First forecast temperature residuals, then use those forecasts to forecast precipitation residuals
# and add the forecast precipitation residuals to the forecast precipitation mean from precip.lm
E_Y.pred <- predict(precip.lm, newdata=next.6mo)^(1/L)
e_t.pred.temp <- forecast(temp.auto, h=6)

next.6mo.temp <- data.frame(time.precip = next.6mo.time, e.ts.temp=e_t.pred.temp$mean)

e_t.pred.precip <- predict(precip.temp.lm, newdata=next.6mo.temp)
next.6mo.prediction <- E_Y.pred + e_t.pred$mean + e_t.pred.precip

# MSE:
mean((next.6mo.prediction-next.6mo$precip)^2)

# Plot actual values and predicted values and confidence intervals
plot(ts(next.6mo$precip),type='o',ylim=c(1,5))
lines(ts(next.6mo.prediction),col='red',type='o')
lines(1:6, E_Y.pred + e_t.pred$lower[,2] + e_t.pred.precip, col = "red", lty = "dashed")
lines(1:6, E_Y.pred + e_t.pred$upper[,2] + e_t.pred.precip, col = "red", lty = "dashed")
legend(1,4, legend = c("Actual", "Predicted"), lwd = 2, col = c("black", "red"))










