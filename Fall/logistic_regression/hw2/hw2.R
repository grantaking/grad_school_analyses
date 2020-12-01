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
ins <- read_sas("insurance_t_bin.sas7bdat")

#check for missing values
sapply(ins,is.na)

#Replace missing values with 99
ins <- data.frame(
  sapply(
    ins,
    function(x) ifelse(is.na(x),
                       99,
                       x)))

#Check crosstabs
sapply(ins, function(x) table(x, ins$INS))

#Aggregate crosstabs where there is separation (only CASHBK and MMCRED)
ins$CASHBK <- as.character(ins$CASHBK)
ins$CASHBK[which(ins$CASHBK > 0)] <- "1 or more"

ins$MMCRED <- as.character(ins$MMCRED)
ins$MMCRED[which(ins$MMCRED > 2)] <- "3+"

#Change variables to factors
ins <- data.frame(
  sapply(
    ins,
    as.factor
  )
)

##Backwards selection using BIC
set.seed(1)
full.model <- glm(INS ~., data = ins, family = binomial(link = "logit"))


back.model <- step(full.model, direction = "backward", k=log(nrow(ins)))
summary(back.model)




##Including interaction terms
#subset dataset to only include variables from backwards selection
ins_subset <- model.frame(back.model)

##Method 1 for detecting quasi-linear separation
#Add two-way interactions with no zero cells to vector
interactions <- vector()
#Looping through the variables twice
for(i in names(ins_subset)) {
  for(j in names(ins_subset)) {
    #If there are no cells with zeros
    if(sum(table(ins_subset[[i]], ins_subset[[j]], ins_subset$INS) == 0) == 0){
      #If the interaction, and it's reverse, aren't already in the list
      if(!paste(i,":",j,sep="") %in% interactions & !paste(j,":",i,sep="") %in% interactions) {
        #Add the interaction to the list
        interactions <- append(interactions,paste(i,":",j, sep=""))
      }
    }
  }
}

##Method 2 for detecting quasi-linear separation
#Run BRGLM with detect separation option
sep <- glm(INS ~ .^2,
           data=ins_subset,
           family=binomial(link="logit"),
           method = "detect_separation",
           linear_program = "dual")
#get terms where there was no separation (zero or NA)
nosep <- names(sep$betas[sep$betas == 0 | is.na(sep$betas)])
#removing numbers and keeping only unique entries
nosep <- unique(gsub('[0-9]+', '', nosep))
#taking only the interactions from this list by detecting the ":"
interactions <- nosep[str_detect(nosep,":")]


#Run forward selection including interactions
empty.model.int <-glm(INS ~ ., data = ins_subset, family = binomial(link="logit"))
full.model.int <- glm(as.formula(paste("INS ~ . + ",paste(interactions, collapse=" + "))), data = ins_subset, family = binomial(link = "logit"))

forward.model <- step(empty.model.int, scope=list(lower=formula(empty.model.int), upper=formula(full.model.int)), direction = "forward", k=log(nrow(ins_subset)))
summary(forward.model)


