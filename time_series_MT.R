# install.packages("forecast")       # install, if necessary
library(forecast)

# Import the data onto a dataframe
df = as.data.frame( read.csv("C:/repos/population_pyramid_analysis/prc_hicp_midx.csv",header=T))

# Remove unused columns
df[9] = NULL
df[1:5] = list(NULL)

# We take data for Malta
df_mt = df[df$geo == "MT",]
df_mt[1] = NULL

df_mt_train = df_mt[1:312,]
df_mt_test = df_mt[313:315,]


# Create a time series object from 2001 to 2021
hicp = ts(as.numeric(as.character(df_mt_train[,2])),start=1996, frequency = 12)
# Test set
hicp_test = ts(as.numeric(as.character(df_mt_test[,2])),start=1996+312/12, frequency = 12)

#examine the time series
plot(hicp, xlab = "Time (year)", ylab = "Harmonized price index")
plot(decompose(hicp))

# Get optimal d values
plot(diff(hicp, differences=1))
abline(a=0, b=0)
plot(diff(hicp, differences=2))
abline(a=0, b=0)
plot(diff(diff(hicp, differences=1), lag=12, differences=1))
abline(a=0, b=0)

var(diff(hicp))
var(diff(hicp, differences=2))
var(diff(diff(hicp, differences=1), lag=12, differences=1))

# d=1, D=1, s=12

# Examine ACF and PACF of differenced series
acf(diff(diff(hicp,differences=1),lag=12,differences=1), lag.max=48, main="")
pacf(diff(diff(hicp,differences=1),lag=12,differences=1),  lag.max=48, main="")


# fit a (0,1,0)x(2,1,1)
arima_1 <- arima (hicp,order=c(0,1,0),
                  seasonal = list(order=c(2,1,1),period=12))
arima_1

# BIC 
# (para AICc: http://stats.stackexchange.com/questions/76761/extract-bic-and-aicc-from-arima-object)
AIC(arima_1,k = log(length(hicp)))   #BIC

# examine ACF and PACF of the residuals
acf(arima_1$residuals, lag.max=48, main="")
pacf(arima_1$residuals, lag.max=48, main="")

# We see a values outside still on lag=2, p=2

# Normality and Constant Variance
plot(arima_1$residuals, ylab = "Residuals")
abline(a=0, b=0)
hist(arima_1$residuals, xlab="Residuals", xlim=c(-1,1))
qqnorm(arima_1$residuals, main="")
qqline(arima_1$residuals)

arima_2 <- arima (hicp,order=c(2,1,0),
                  seasonal = list(order=c(2,1,1),period=12))
arima_2

AIC(arima_2,k = log(length(hicp)))   #BIC

# examine ACF and PACF of the residuals
acf(arima_2$residuals, lag.max=48, main="")
pacf(arima_2$residuals, lag.max=48, main="")


# Normality and Constant Variance
plot(arima_2$residuals, ylab = "Residuals")
abline(a=0, b=0)
hist(arima_2$residuals, xlab="Residuals", xlim=c(-1,1))
qqnorm(arima_2$residuals, main="")
qqline(arima_2$residuals)

#Comparamos con auto arima

arima_auto = auto.arima(hicp,d=1,D=1,max.order=8,trace=TRUE,approx=FALSE,allowdrift=FALSE,stepwise=FALSE)
arima_auto

# examine ACF and PACF of the residuals
acf(arima_auto$residuals, lag.max=48, main="")
pacf(arima_auto$residuals, lag.max=48, main="")


AIC(arima_auto,k = log(length(hicp)))   #BIC



# Normality and Constant Variance
plot(arima_auto$residuals, ylab = "Residuals")
abline(a=0, b=0)
hist(arima_auto$residuals, xlab="Residuals", xlim=c(-1,1))
qqnorm(arima_auto$residuals, main="")
qqline(arima_auto$residuals)

arima_no_stationality <- arima (hicp,order=c(2,1,0))
arima_no_stationality

model = arima_no_stationality

model.predict <- predict(model,n.ahead=3)
plot(hicp, xlim=c(2021,2022.4),
     xlab = "Time (years)",
     ylab = "Harmonized price index",
     ylim=c(80,115))
lines(model.predict$pred,col=2)
lines(model.predict$pred+1.96*model.predict$se, col=3, lty=2)
lines(model.predict$pred-1.96*model.predict$se, col=3, lty=2)
lines(hicp_test,col=4)

hicp_test

