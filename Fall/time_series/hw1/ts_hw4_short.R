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

#Visualizing - due to visualization, we conclude that using a linear term is inappropriate since the trend is not consistent throughout the period.
#We considered using a quadratic trend, but online resources cautioned against using quadratic trends for real-world time-series data, as they are
#likely to lead to inaccurate (explosive) estimates unless you have domain knowledge that suggests that a quadratic trend is appropriate.
#Therefore, we settled on no trend, both for realism and model parsimony.
decomp_stl <- stl(ts, s.window=7)
plot(decomp_stl)


#NSDIFFS indicates that no seasonal differences are needed for this time series
nsdiffs(ts)

#fit deterministic seasonal arima with fourier
arima<-Arima(training,order=c(0,0,0),xreg=fourier(training,K=4))
summary(arima)
resid <- arima$residuals
Acf(resid)
Pacf(resid)
White.LB <- rep(NA, 20)
for(i in 1:20){
  White.LB[i] <- Box.test(resid, lag = i, type = "Lj", fitdf = 0)$p.value
}
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 1))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

#Due to ACF and PACF plots, as well as the lack of white noise, we tried fitting AR and MA terms.
#We tried fitting (1,0,2), (1,0,0), and (0,0,2) models. Code here is shown for the (1,0,0) model, which we eventually chose
lev_arima = Arima(training,xreg=fourier(training,K=4), order=c(1,0,0), seasonal=c(0,0,0))
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


###Prediction
#We had to manually create the fourier transformation here, since using it shouldn't start at x=0
x=seq(55,60)
x1.sin=sin(2*pi*x*1/12)
x1.cos=cos(2*pi*x*1/12)
x2.sin=sin(2*pi*x*2/12)
x2.cos=cos(2*pi*x*2/12)
x3.sin=sin(2*pi*x*3/12)
x3.cos=cos(2*pi*x*3/12)
x4.sin=sin(2*pi*x*4/12)
x4.cos=cos(2*pi*x*4/12)

x.reg.lev.valid =cbind(x1.sin,x1.cos,x2.sin,x2.cos,x3.sin,x3.cos,x4.sin,x4.cos)

lev_predict <- forecast(lev_arima, h=6, xreg=x.reg.lev.valid)
ValidationPlot_lev <- data.frame(Y=as.matrix(test), date=as.Date(as.yearmon(time(test))))
ValidationPlot_lev <- cbind(ValidationPlot_lev,lev_predict) %>%
  select(-c("Lo 80","Hi 80"))

brks2 <- monthly$Date[seq(55, length(monthly$Date))]
lbls2 <- paste0(lubridate::month(brks2, label=TRUE, abbr=TRUE), " ", lubridate::year(brks2))

ggplot(ValidationPlot_lev, aes(x=date)) +
  geom_line(aes(y=`Point Forecast`, linetype="Predicted"), size=1, color="#014d64") +
  geom_point(aes(y=Y, color="Observed"), shape=1, size = 2, stroke=2) +
  geom_ribbon(aes(ymin=`Lo 95`,ymax=`Hi 95`, fill="95% CI"), alpha=0.3) +
  labs(title="Time Plot: Predicted vs. Observed PM 2.5 Concentration, Level ARIMA", x="", y="PM 2.5 Concentration (μg/m^3)") +
  scale_x_date(labels = lbls2, breaks = brks2) +
  theme_economist() + scale_color_manual("", values="#01a2d9") + scale_fill_economist("") + scale_linetype_discrete("") +
  theme(plot.title = element_text(hjust = 0.5))

lev_mape = colMeans(abs((ValidationPlot_lev['Point Forecast'] - ValidationPlot_lev$Y))/ValidationPlot_lev$Y)
# The Final MAPE for the (1,0,0) model was .1394, compared to 0.1367 for the (1,0,2) model
# The BIC for the (1,0,0) model was lower than for the (1,0,2) model
# For parsimony, we chose the (1,0,0) model
lev_mape
