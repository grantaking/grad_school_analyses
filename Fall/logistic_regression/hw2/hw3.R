library(MASS)
library(car)
library(rJava)
library(glmulti)
library(givitiR)
library(haven)
library(brglm2)
library(stringr)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(DescTools)

setwd("C:/Users/grant/Documents/MSA/Fall/logistic_regression/hw2")

#### HW 3 #######################################
#dataset
train <- read_sas("insurance_t_bin.sas7bdat")

#Replace missing values with 99
train <- data.frame(
  sapply(
    train,
    function(x) ifelse(is.na(x),
                       99,
                       x)))


#Change variables to factors
train <- data.frame(
  sapply(
    train,
    as.factor
  )
)

#Final model from HW 2
final.model <- glm(formula = INS ~ DDA + NSF + IRA + INV + ILS + MM + MTG + 
                     CC + DDABAL_Bin + CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + 
                     ATMAMT_Bin + CDBAL_Bin + DDA:IRA, family = binomial(link = "logit"), 
                   data = train)



#Discrimination slope
train$p_hat <- predict(final.model, type="response")
p1 <- train$p_hat[train$INS == 1]
p0 <- train$p_hat[train$INS == 0]
coef_discrim <- mean(p1) - mean(p0)

ggplot(train, aes(p_hat, fill = factor(INS))) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability",
       fill = "Outcome",
       title = paste("Coefficient of Discrimination = ",
                     round(coef_discrim, 3), sep = ""))

#Concordance
Concordance(train$INS, train$p_hat)

#ROC Curve
pred <- prediction(fitted(final.model), factor(train$INS))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)

performance(pred, measure = "auc")@y.values

#KS Statistic
ks_plot(train$INS, train$p_hat)
ks_stat(train$INS, train$p_hat)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
KS <- max(perf@y.values[[1]] - perf@x.values[[1]])
cutoffAtKS <- unlist(perf@alpha.values)[which.max(perf@y.values[[1]] - perf@x.values[[1]])]
print(c(KS, cutoffAtKS))

plot(x = unlist(perf@alpha.values), y = (1-unlist(perf@y.values)),
     type = "l", main = "K-S Plot (EDF)",
     xlab = 'Cut-off',
     ylab = "Proportion",
     col = "red")
lines(x = unlist(perf@alpha.values), y = (1-unlist(perf@x.values)), col = "blue")


####### Validation dataset
valid <- read_sas("insurance_v_bin.sas7bdat")

#Replace missing values with 99
valid <- data.frame(
  sapply(
    valid,
    function(x) ifelse(is.na(x),
                       99,
                       x)))

#Change variables to factors
valid <- data.frame(
  sapply(
    valid,
    as.factor
  )
)

valid$p_hat <- predict(final.model, valid, type="response")

#Confusion Matrix
cm <- confusionMatrix(valid$INS, valid$p_hat, threshold = cutoffAtKS)
cm

#Accuracy
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
accuracy

#Lift - Training
perf <- performance(pred, measure = "lift", x.measure = "rpp")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Training Data")
abline(h = 1, lty = 3)

#Lift - Validation
pred_v <- prediction(predict(final.model, newdata=valid, type="response"), factor(valid$INS))
perf_v <- performance(pred_v, measure = "lift", x.measure = "rpp")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE,
     colorize.palette = rev(gray.colors(256)),
     main = "Lift Chart for Validation Data")
abline(h = 1, lty = 3)


#Lift (In validation dataset) for cutoff based on KS Statistic
depth <- sum(cm[2,])
ppv <- cm[2,2] / depth
lift <- ppv / mean(as.numeric(as.character(valid$INS)))

