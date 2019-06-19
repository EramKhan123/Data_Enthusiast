#Load library
library(ggplot2)
library(forecast)
library(fpp)
library(fpp2)
library(urca)
########################################read and create a time series#####################

Dow_jones <- read.csv("C:/Users/ek639/OneDrive/Desktop/Rutgers_BF/bf_final/DJ.csv")
View(Dow_jones)
Dow_jones1 <- ts(Dow_jones$Adj.Close, start=c(2000, 1), freq=12)

#############################plot the timeseries graph and acf graph#########################

plot(Dow_jones1)
ggAcf(Dow_jones1)
ggPacf(Dow_jones1)
#we can see the trend but there is no seasonaliy.

#############################test stationary or non satationary##############################

test_stationary = ur.kpss(Dow_jones1)
test_stationary
summary(test_stationary)

#found the data is non stationary its dependent on time

######################Decomposition to understand the trend and seasonality in the data############

fit_stl <- stl(Dow_jones1, s.window=1)
plot(fit_stl)
fit_stl$time.series
plot(Dow_jones1, col="gray",main="Dow Jones Index",ylab="Adujusted Closing Price", xlab="")


monthplot(fit_stl$time.series[,"seasonal"], main="", ylab="Seasonal")
#how to interpret this


################################ Simple Forecasting Techniques ###################################
################################Without Adjusting Seasonality and trend############################
dowjones_train<-ts(Dow_jones1, start=c(2000, 1), end=c(2017, 12), freq=12)
dowjones_train
dowjones_test <- ts(Dow_jones1, start=c(2018, 1), end=c(2018, 12), freq=12)


dow_mean <-meanf(dowjones_train,h=12)
plot(dow_mean)
dow_mean

dow_naive <- naive(dowjones_train, h=12)
dow_naive

dow_snaive <- snaive(dowjones_train, h=12)
dow_snaive

dow_drift<-rwf(dowjones_train,h=12,drift=TRUE)
dow_drift

Ac_mean<-accuracy(dow_mean, dowjones_test)
Ac_mean
#to find mean square error
Acmean<-Ac_mean[4]*Ac_mean[4]
Ac_mean

Ac_naive<-accuracy(dow_naive,dowjones_test)
Acnaive<-Ac_naive[4]*Ac_naive[4]

Ac_snavie<-accuracy(dow_snaive,dowjones_test)
Acsnaive<-Ac_snavie[4]*Ac_snavie[4]

Ac_drift<-accuracy(dow_drift,dowjones_test)
Acdrift<-Ac_drift[4]*Ac_drift[4]

autoplot(dowjones_train) +
  autolayer(dow_mean, series="Mean", PI=FALSE) +
  autolayer(dow_naive, series="Naïve", PI=FALSE) +
  autolayer(dow_snaive, series="Seasonal Naïve", PI=FALSE) +
  autolayer(dow_drift, series="Drift", PI=FALSE) +
  xlab("Monthly") + ylab("Adjusted Closing Price") +
  ggtitle("DOW JONES INEDEX") +
  guides(colour=guide_legend(title="Forecast"))

err<-c(Acmean,Acnaive,Acsnaive,Acdrift)
barplot(err,names.arg = c("mean","naive","Seasonal Naive","Drift"),col = "green")

################################################Models on Non Stationary Timeseries#############################

                                                 ##HOlt(works only on trend)##
#Holt works on only trend
#Not sure if the seasonality is there in the data,if seasonality is there then we will use  holt winter model
holt_fit<-holt(dowjones_train,h=12)
fit_holt<-forecast(holt_fit,h=12)
Ac_holt<-accuracy(fit_holt,dowjones_test)
Holt_mse<-Ac_holt[4]*Ac_holt[4]
autoplot(dowjones_train) +
  autolayer(fit_holt, series="Holt", PI=FALSE) 
                                             
                                                  ##Holt Winter model(works on trend and Seasonality)##
                                                  ##Additive##
hwa<- hw(dowjones_train,seasonal="additive")
fit_hwa<-forecast(hwa,h=12)
Ac_hwa<-accuracy(fit_hwa,dowjones_test)
Hwa_mse<-Ac_hwa[4]*Ac_hwa[4]
autoplot(dowjones_train) +
  autolayer(fit_hwa, series="Holt Winter Additive", PI=FALSE) 
                                                 ##Holt Winter model(works on trend and Seasonality)##
                                                    ##Multiplicative##
hwm<- hw(dowjones_train,seasonal="multiplicative")
fit_hwm<-forecast(hwm,h=12)
Ac_hwm<-accuracy(fit_hwm,dowjones_test)
Hwm_mse<-Ac_hwm[4]*Ac_hwm[4]

autoplot(dowjones_train) +
  autolayer(fit_hwm, series="Holt Winter Multiplicative", PI=FALSE) 

##ARMA##to be used
#find p
#check accuracy
arma_fit<-arima(dowjones_train,order=c(2,0,2))

#####################################################MAking time Series Stationary ###################################

ndiffs(Dow_jones1)
dowjones_stationary<-diff(Dow_jones1)

plot(dowjones_stationary)
plot(acf(dowjones_stationary))
plot(pacf(dowjones_stationary))


#################################################Arima model:works on stationary model############################################################################################
dowjones_train_stationary<-ts(dowjones_stationary, start=c(2000, 1), end=c(2017, 12), freq=12)
dowjones_test_stationary <- ts(dowjones_stationary, start=c(2018, 1), end=c(2018, 12), freq=12)

##Best model was found out to be with p=2 d=1 and q=1
bestmodel <-auto.arima(dowjones_train_stationary,seasonal=FALSE)

arima_model <- Arima(dowjones_train_stationary, order = c(2,1,1),include.drift = TRUE)
summary(arima_model)
fit1<-forecast(arima_model,h=12)
accuracy(fit1,dowjones_test_stationary)

autoplot(dowjones_train_stationary) +
  autolayer(fit1, series="Arima model", PI=FALSE) 


