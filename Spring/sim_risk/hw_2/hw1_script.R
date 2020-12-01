#mport/some cleaning

## @knitr hw1_results
library(car)
library(caret)
library(tidyr)
library(dplyr)
library(stats)
library(readxl)
library(ggplot2)
library(clipr)
library(ks)
library(EnvStats)
library(triangle)
library(knitr)

projections <- read_excel("C:/Users/grant/Documents/MSA/Spring/sim_risk/hw_2/Analysis_Data.xlsx", sheet = "Price Projections")
 


costs <- read_excel("C:/Users/grant/Documents/MSA/Spring/sim_risk/hw_2/Analysis_Data.xlsx", sheet = "Drilling Cost")

costs[,2:7] <- sapply(costs[,2:7], as.numeric)

 


 
costs.sub <- costs[32:47,]
costs.sub[,2:7] <- sapply(costs.sub[,2:7], as.numeric)
 

 
costs_vec <- c(costs.sub[["Arithmetic Return - Crude Oil"]], costs.sub[["Arithmetic Return - Natural Gas"]], costs.sub[["Arithmetic Return - Dry Well"]])

density.costs <- density(costs_vec, "SJ-ste")
density.costs

set.seed(12345)
est.density.costs <- rkde(fhat = kde(costs_vec,h = 0.07935), n = 10000)
hist(est.density.costs, breaks = 50, main = 'Estimated Value Distribution', xlab = "value")
 
   
P2006 <- rowMeans(costs[47,2:4])
qqnorm( y = est.density.costs, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)  
qqline(y = est.density.costs) 
  
set.seed(12345)

mu = mean(costs_vec)
stdev = sd(costs_vec)

norm_cost_2016 <- rep(0,10000)

for(i in 1:10000) {
  Pt <- P2006 # average 'Price' today
  
  for(j in 2007:2016) {
    r <- rnorm(n=1,mean=mu, sd=stdev)
    Pt <- Pt*(1+r) 
  }
  
  norm_cost_2016[i] <- Pt
} 

 

 
set.seed(12345)
  
norm_cost_2020 <- rep(0,10000)

for(i in 1:10000) {
  Pt <- P2006
  
  for(j in 2007:2012) {
    r <- rnorm(n=1,mean=mu, sd=stdev)
    Pt <- Pt*(1+r)
  }
  
  for(j in 2013:2015) {
    r <- rtriangle(n=1, a=-.22, b=-.07, c=-.0917)
    Pt <- Pt*(1+r)
  }
  
  for(j in 2016:2020) {
    r <- rtriangle(n=1, a=.02, b=.06, c=.05)
    Pt <- Pt*(1+r)
  }
  
norm_cost_2020[i] <- Pt
}

  


 
qqnorm(y = norm_cost_2020,  main = "Normal Q-Q Plot - Simulated Changes (2006 - 2012)",
          xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
          plot.it = TRUE, datax = FALSE)  
qqline(y = norm_cost_2020)
 

    
set.seed(12345)
nonnorm_cost_2020 <- rep(0,10000)

for(i in 1:10000) {
  Pt <- P2006
  
  for(j in 2007:2012) {
    r <- rkde(fhat=kde(costs_vec, h=0.07935), n=1)
    Pt <- Pt*(1+r)
  }
  
  for(j in 2013:2015) {
    r <- rtriangle(n=1, a=-.22, b=-.07, c=-.0917)
    Pt <- Pt*(1+r)
  }
  
  for(j in 2016:2020) {
    r <- rtriangle(n=1, a=.02, b=.06, c=.05)
    Pt <- Pt*(1+r)
  }
  
  nonnorm_cost_2020[i] <- Pt
}

 


hist(nonnorm_cost_2020, breaks = 50, main = 'Estimated 2020 Cost Distribution - KDE', xlab = "Dollars" )
abline(v = P2006, col="red", lwd = 2)
mtext("2006 Avg. Cost", at=P2006, col="red")

mean(nonnorm_cost_2020)
sd(nonnorm_cost_2020)

 