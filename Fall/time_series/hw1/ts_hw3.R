library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(padr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(xts)
library(foreign)
library(tidyr)
library(ggthemes)
library(uroot)
library(tseries)
library(reshape2)



setwd("C:\\Users\\grant\\Documents\\MSA\\Fall\\time_series\\hw1")
df <- read.csv("PM_2_5_Raleigh2.csv")



#generating rows with missing values for missing dates
df$Date<-as.Date(df$Date,format="%m/%d/%Y")
df1 <- pad(df) #from padr package

#counting missing values
print(sum(is.na(df1$Daily.Mean.PM2.5.Concentration)))

#aggregating to monthly
df1$month <- month(df1$Date)
df1$year <- year(df1$Date)
monthly <- aggregate(df1, list(df1$month,df1$year), mean, na.rm = TRUE)
ts <- ts(monthly$Daily.Mean.PM2.5.Concentration, start=2014, frequency = 12)


training=subset(ts,end=length(ts)-6)
test=subset(ts,start=length(ts)-5)


decomp_stl <- stl(ts, s.window=7)
plot(decomp_stl)

#no differences needed according to nsdiffs
nsdiffs(ts)

#fit deterministic seasonal arima with fourier
arima<-Arima(training,order=c(0,0,0),xreg=fourier(training,K=4))
summary(arima)

#take residuals and look for higher order trends
resid <- arima$residuals
plot(resid)
x <- seq(1:54)
x2 <- x**2
x3 <- x**3
resid_x <- cbind(resid,x,x2,x3)

#Developing ARIMA Models################################
##Linear Model
lin_model <- lm(resid ~ x, data=resid_x)
lin_resid <- lin_model$residuals
#Dickey FUller tests
adf.test(lin_resid, alternative = "stationary", k=0)
adf.test(lin_resid, alternative = "stationary", k=1)
adf.test(lin_resid, alternative = "stationary", k=2)
#Fitting final ARIMA
lin_resid.ts <- ts(lin_resid,frequency=12)
lin_arima <- auto.arima(lin_resid.ts)
lin_arima_resid <- lin_arima$residuals
#ACF and PACF plots
acf(lin_arima_resid)
pacf(lin_arima_resid)
# Ljung-Box Test for ARIMA Residuals #
White.LB_lin <- rep(NA, 20)
for(i in 1:20){
  White.LB_lin[i] <- Box.test(lin_arima_resid, lag = i, type = "Lj", fitdf = 0)$p.value
}
barplot(White.LB_lin, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 1))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

##Quadratic Model
quad_model <- lm(resid ~ x+x2, data=resid_x)
quad_resid <- quad_model$residuals
#Dickey FUller tests
adf.test(quad_resid, alternative = "stationary", k=0)
adf.test(quad_resid, alternative = "stationary", k=1)
adf.test(quad_resid, alternative = "stationary", k=2)
#Fitting final ARIMA
quad_resid.ts <- ts(quad_resid, frequency=12)
quad_arima <- auto.arima(quad_resid.ts)
quad_arima_resid <- quad_arima$residuals
#ACF and PACF plots
acf(quad_arima_resid)
pacf(quad_arima_resid)
# Ljung-Box Test for ARIMA Residuals #
White.LB_quad <- rep(NA, 20)
for(i in 1:20){
  White.LB_quad[i] <- Box.test(quad_arima_resid, lag = i, type = "Lj", fitdf = 0)$p.value
}
barplot(White.LB_quad, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 1))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

##Cubic Model
cub_model <- lm(resid ~ x+x2+x3, data=resid_x)
cub_resid <- cub_model$residuals
#Dickey FUller tests
adf.test(cub_resid, alternative = "stationary", k=0)
adf.test(cub_resid, alternative = "stationary", k=1)
adf.test(cub_resid, alternative = "stationary", k=2)
#Fitting final ARIMA
cub_resid.ts <- ts(cub_resid, frequency=12)
cub_arima <- auto.arima(cub_resid.ts)
cub_arima_resid <- cub_arima$residuals
#ACF and PACF plots
acf(cub_arima_resid)
pacf(cub_arima_resid)
# Ljung-Box Test for ARIMA Residuals #
White.LB_cub <- rep(NA, 20)
for(i in 1:20){
  White.LB_cub[i] <- Box.test(cub_arima_resid, lag = i, type = "Lj", fitdf = 0)$p.value
}
barplot(White.LB_cub, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 1))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")




###Creating a Single ARIMA with seasonality and trend (for prediction)
###Creating Covariates
x=seq(1,length(training))
x2 = x**2
x3 = x**3
x1.sin=sin(2*pi*x*1/12)
x1.cos=cos(2*pi*x*1/12)
x2.sin=sin(2*pi*x*2/12)
x2.cos=cos(2*pi*x*2/12)
x3.sin=sin(2*pi*x*3/12)
x3.cos=cos(2*pi*x*3/12)
x4.sin=sin(2*pi*x*4/12)
x4.cos=cos(2*pi*x*4/12)

x.reg.lev = cbind(x1.sin,x1.cos,x2.sin,x2.cos,x3.sin,x3.cos,x4.sin,x4.cos)
x.reg.lin =cbind(x,x1.sin,x1.cos,x2.sin,x2.cos,x3.sin,x3.cos,x4.sin,x4.cos)
x.reg.quad =cbind(x,x2,x1.sin,x1.cos,x2.sin,x2.cos,x3.sin,x3.cos,x4.sin,x4.cos)
x.reg.cub=cbind(x,x2,x3,x1.sin,x1.cos,x2.sin,x2.cos,x3.sin,x3.cos,x4.sin,x4.cos)

#Level Arima
lev_arima = Arima(training,xreg=x.reg.lev, order=c(1,0,0), seasonal=c(0,0,0))
lev_arima_resid = lev_arima$residuals
Acf(lev_arima_resid)
Pacf(lev_arima_resid)
White.LB_lev <- rep(NA, 20)
for(i in 1:20){
  White.LB_lev[i] <- Box.test(lev_arima_resid, lag = i, type = "Lj", fitdf = 0)$p.value
}
barplot(White.LB_lev, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 1))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

#Linear Arima
lin_arima = Arima(training,xreg=x.reg.lin, order=c(2,0,0), seasonal=c(1,0,1))
lin_arima_resid = lin_arima$residuals
acf(lin_arima_resid)
pacf(lin_arima_resid)
White.LB_lin <- rep(NA, 20)
for(i in 1:20){
  White.LB_lin[i] <- Box.test(lin_arima_resid, lag = i, type = "Lj", fitdf = 0)$p.value
}
barplot(White.LB_lin, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 1))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

#Quadratic Arima
quad_arima = Arima(training,xreg=x.reg.quad, order=c(2,0,0), seasonal=c(1,0,1))
quad_arima_resid = quad_arima$residuals
acf(quad_arima_resid)
pacf(quad_arima_resid)
White.LB_quad <- rep(NA, 20)
for(i in 1:20){
  White.LB_quad[i] <- Box.test(quad_arima_resid, lag = i, type = "Lj", fitdf = 0)$p.value
}
barplot(White.LB_quad, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 1))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

#Cubic Arima
cub_arima = Arima(training,xreg=x.reg.cub, order=c(2,0,0), seasonal=c(1,0,1))
cub_arima_resid = cub_arima$residuals
acf(cub_arima_resid)
pacf(cub_arima_resid)
White.LB_cub <- rep(NA, 20)
for(i in 1:20){
  White.LB_cub[i] <- Box.test(cub_arima_resid, lag = i, type = "Lj", fitdf = 0)$p.value
}
barplot(White.LB_cub, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 1))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


###Prediction
x=seq(55,60)
x2 = x**2
x3 = x**3
x1.sin=sin(2*pi*x*1/12)
x1.cos=cos(2*pi*x*1/12)
x2.sin=sin(2*pi*x*2/12)
x2.cos=cos(2*pi*x*2/12)
x3.sin=sin(2*pi*x*3/12)
x3.cos=cos(2*pi*x*3/12)
x4.sin=sin(2*pi*x*4/12)
x4.cos=cos(2*pi*x*4/12)

x.reg.lev.valid =cbind(x1.sin,x1.cos,x2.sin,x2.cos,x3.sin,x3.cos,x4.sin,x4.cos)
x.reg.lin.valid =cbind(x,x1.sin,x1.cos,x2.sin,x2.cos,x3.sin,x3.cos,x4.sin,x4.cos)
x.reg.quad.valid =cbind(x,x2,x1.sin,x1.cos,x2.sin,x2.cos,x3.sin,x3.cos,x4.sin,x4.cos)
x.reg.cub.valid =cbind(x,x2,x3,x1.sin,x1.cos,x2.sin,x2.cos,x3.sin,x3.cos,x4.sin,x4.cos)

brks2 <- monthly$Date[seq(55, length(monthly$Date))]
lbls2 <- paste0(lubridate::month(brks2, label=TRUE, abbr=TRUE), " ", lubridate::year(brks2))

#Level Arima
lev_predict <- forecast(lev_arima, h=6, xreg=x.reg.lev.valid)
ValidationPlot_lev <- data.frame(Y=as.matrix(test), date=as.Date(as.yearmon(time(test))))
ValidationPlot_lev <- cbind(ValidationPlot_lev,lev_predict) %>%
  select(-c("Lo 80","Hi 80"))
ggplot(ValidationPlot_lev, aes(x=date)) +
  geom_line(aes(y=`Point Forecast`, linetype="Predicted"), size=1, color="#014d64") +
  geom_point(aes(y=Y, color="Observed"), shape=1, size = 2, stroke=2) +
  geom_ribbon(aes(ymin=`Lo 95`,ymax=`Hi 95`, fill="95% CI"), alpha=0.3) +
  labs(title="Time Plot: Predicted vs. Observed PM 2.5 Concentration, Level ARIMA", x="", y="PM 2.5 Concentration (??g/m^3)") +
  scale_x_date(labels = lbls2, breaks = brks2) +
  theme_economist() + scale_color_manual("", values="#01a2d9") + scale_fill_economist("") + scale_linetype_discrete("") +
  theme(plot.title = element_text(hjust = 0.5))

lev_mape = colMeans(abs((ValidationPlot_lev['Point Forecast'] - ValidationPlot_lev$Y))/ValidationPlot_lev$Y)
lev_mape

#Linear Arima
lin_predict <- forecast(lin_arima, h=6, xreg=x.reg.lin.valid)
ValidationPlot_lin <- data.frame(Y=as.matrix(test), date=as.Date(as.yearmon(time(test))))
ValidationPlot_lin <- cbind(ValidationPlot_lin,lin_predict) %>%
  select(-c("Lo 80","Hi 80"))
ggplot(ValidationPlot_lin, aes(x=date)) +
  geom_line(aes(y=`Point Forecast`, linetype="Predicted"), size=1, color="#014d64") +
  geom_point(aes(y=Y, color="Observed"), shape=1, size = 2, stroke=2) +
  geom_ribbon(aes(ymin=`Lo 95`,ymax=`Hi 95`, fill="95% CI"), alpha=0.3) +
  labs(title="Time Plot: Predicted vs. Observed PM 2.5 Concentration, Linear ARIMA", x="", y="PM 2.5 Concentration (??g/m^3)") +
  scale_x_date(labels = lbls2, breaks = brks2) +
  theme_economist() + scale_color_manual("", values="#01a2d9") + scale_fill_economist("") + scale_linetype_discrete("") +
  theme(plot.title = element_text(hjust = 0.5))


#Quadratic Arima
quad_predict <- forecast(quad_arima, h=6, xreg=x.reg.quad.valid)
ValidationPlot_quad <- data.frame(Y=as.matrix(test), date=as.Date(as.yearmon(time(test))))
ValidationPlot_quad <- cbind(ValidationPlot_quad,quad_predict)  %>%
  select(-c("Lo 80","Hi 80"))
ggplot(ValidationPlot_quad, aes(x=date)) +
  geom_line(aes(y=`Point Forecast`, linetype="Predicted"), size=1, color="#014d64") +
  geom_point(aes(y=Y, color="Observed"), shape=1, size = 2, stroke=2) +
  geom_ribbon(aes(ymin=`Lo 95`,ymax=`Hi 95`, fill="95% CI"), alpha=0.3) +
  labs(title="Time Plot: Predicted vs. Observed PM 2.5 Concentration, Quadratic ARIMA", x="", y="PM 2.5 Concentration (??g/m^3)") +
  scale_x_date(labels = lbls2, breaks = brks2) +
  theme_economist() + scale_color_manual("", values="#01a2d9") + scale_fill_economist("") + scale_linetype_discrete("") +
  theme(plot.title = element_text(hjust = 0.5))

#Cubic Arima
cub_predict <- forecast(cub_arima, h=6, xreg=x.reg.cub.valid)
ValidationPlot_cub <- data.frame(Y=as.matrix(test), date=as.Date(as.yearmon(time(test))))
ValidationPlot_cub <- cbind(ValidationPlot_cub,cub_predict)  %>%
  select(-c("Lo 80","Hi 80"))
ggplot(ValidationPlot_cub, aes(x=date)) +
  geom_line(aes(y=`Point Forecast`, linetype="Predicted"), size=1, color="#014d64") +
  geom_point(aes(y=Y, color="Observed"), shape=1, size = 2, stroke=2) +
  geom_ribbon(aes(ymin=`Lo 95`,ymax=`Hi 95`, fill="95% CI"), alpha=0.3) +
  labs(title="Time Plot: Predicted vs. Observed PM 2.5 Concentration, Cubic ARIMA", x="", y="PM 2.5 Concentration (??g/m^3)") +
  scale_x_date(labels = lbls2, breaks = brks2) +
  theme_economist() + scale_color_manual("", values="#01a2d9") + scale_fill_economist("") + scale_linetype_discrete("") +
  theme(plot.title = element_text(hjust = 0.5))
