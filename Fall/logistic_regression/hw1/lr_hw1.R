library(MASS)
library(visreg)
library(brglm)
library(car)
library(mgcv)
library(haven)


setwd("C:\\Users\\grant\\Documents\\MSA\\Fall\\logistic_regression\\hw1")

train <- read_sas(data_file = "insurance_t.sas7bdat")

#Variance Inflation Factor

logit.model <- glm(INS ~ ACCTAGE + DDA + DDABAL + DEP + DEPAMT + CASHBK + CHECKS +
            DIRDEP + NSF + NSFAMT + PHONE + TELLER + SAV + SAVBAL +
            ATM + ATMAMT + POS + POSAMT + CD + CDBAL + IRA + IRABAL +
            LOC + LOCBAL + INV + INVBAL + ILS + ILSBAL + MM + MMBAL +
            MMCRED + MTG + MTGBAL + CC + CCBAL + CCPURC + SDB + INCOME +
            HMOWN + LORES + HMVAL + AGE + CRSCORE + MOVED + INAREA +
            BRANCH + RES, data=train, family=binomial(link="logit"))

vif(logit.model)

cor(train$MM, train$MMBAL)
cor(train$ILS, train$ILSBAL)
cor(train$MTGBAL, train$CCBAL, use="complete.obs")


