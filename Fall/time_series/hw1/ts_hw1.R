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
library(extrafont)


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
monthly2 <- monthly %>% select(Date= string(Date), PM2point5 = Daily.Mean.PM2.5.Concentration)

###########################################Exporting for SAS Analysis
write.csv(x=monthly2,file="C:\\Users\\grant\\Documents\\MSA\\Fall\\time_series\\hw1\\monthly.csv")





## Visualization
#creating time series object for decomposition
ts <- ts(monthly$Daily.Mean.PM2.5.Concentration, start=2014, frequency = 12)
decomp_stl <- stl(ts, s.window=7)
plot(decomp_stl)
decomp_df <- as.data.frame(decomp_stl$time.series)
decomp_df$date <- seq(as.Date("2014-01-16"), as.Date("2018-12-16"), by='mon')
decomp_df$observed <- decomp_df$seasonal + decomp_df$trend + decomp_df$remainder
decomp_df$adjusted <- decomp_df$observed - decomp_df$seasonal

decomp_t <- decomp_df %>%
  select(date,trend,observed) %>%
  gather(key="Variable",value="value", -date) %>%
  mutate(Variable = factor(Variable))
                           
decomp_s <- decomp_df %>%
  select(date,adjusted,observed) %>%
  gather(key="Variable",value="value", -date) %>%
  mutate(Variable = factor(Variable)) %>%
  mutate(Variable = factor(Variable, levels = rev(levels(Variable))))


brks <- monthly$Date[seq(1, length(monthly$Date), 12)]
lbls <- lubridate::year(brks)


# Trend/Cycle
ggplot(decomp_t, aes(x=date, y=value, color=Variable)) +
  geom_line(size = 1) +
  labs(title="PM 2.5 Trend Component vs. Observed Values", x="", y="PM 2.5 Concentration (??g/m^3)") +
  scale_x_date(labels = lbls, breaks = brks) +
  theme_economist() + scale_color_economist("",labels=c("Observed","Trend")) +
  theme(plot.title = element_text(hjust = 0.5))
  

# Seasonally Adjusted
ggplot(decomp_s, aes(x=date, y=value, color=Variable)) +
  geom_line(size = 1) +
  labs(title="PM 2.5 Seasonally Adjusted vs. Observed Values", x="", y="PM 2.5 Concentration (??g/m^3)") +
  scale_x_date(labels = lbls, breaks = brks) +
  theme_economist() + scale_color_economist("",labels=c("Observed","Seasonally Adjusted")) +
  theme(plot.title = element_text(hjust = 0.5))



################################Importing the predictions from the SAS model
windowsFonts(tw_cen_mt = windowsFont("Tw Cen MT Regular"))
font_import("Tw Cen MT Regular")
# Time Plot
timeplot_df <- read.csv(file="predict.csv") %>%
  filter(X_TIMEID_ >= 55) %>%
  select(date = X_TIMEID_, ACTUAL, PREDICT, LOWER, UPPER) %>%
  mutate(date = monthly$Date[seq(55, length(monthly$Date))])

brks2 <- monthly$Date[seq(55, length(monthly$Date))]
lbls2 <- paste0(lubridate::month(brks2, label=TRUE, abbr=TRUE), " ", lubridate::year(brks2))

ggplot(timeplot_df, aes(x=date)) +
  geom_line(aes(y=PREDICT, linetype="Predicted"), size=1, color="#014d64") +
  geom_point(aes(y=ACTUAL, color="Observed"), shape=1, size = 2, stroke=2) +
  geom_ribbon(aes(ymin=LOWER,ymax=UPPER, fill="95% CI"), alpha=0.3) +
  labs(title="Time Plot: Predicted vs. Observed PM 2.5 Concentration", x="", y="PM 2.5 Concentration (??g/m^3)") +
  scale_x_date(labels = lbls2, breaks = brks2) +
  theme_economist(base_family = "tw_cen_mt") + scale_color_manual("", values="#01a2d9") + scale_fill_economist("") + scale_linetype_discrete("") +
  theme(plot.title = element_text(hjust = 0.5))


