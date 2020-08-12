rm(list = ls())

# read data
data<-read.csv(file = "4d analysis.csv")

sum(is.na(data))

str(data)

colnames(data)[1]<-c("Group")


## install packages
#install.packages('glmnet')
#install.packages('MASS')
#install.packages('survival')
#install.packages('rms')
#install.packages('car')
#install.packages('ggplot2')
#install.packages('survminer')
#install.packages('ggridges')
#install.packages('pROC')
#install.packages("plotROC")
#install.packages("riskRegression")

library(car)
library(ggplot2)
library(glmnet)
library(MASS)
library(survival)
library(rms)
library(survminer)
library(ggridges)
library(pROC)
library(plotROC)
library(riskRegression)


N_folds <- 10

# randomly assign patients into N groups of equal size 
data$random <- sample(1:nrow(data), nrow(data), replace=F)
data$group <- data$random %% N_folds + 1

table(data$group)

c_test <- 0
brier_test <- 0


for (i in 1:N_folds){
  
  data.train <- subset(data,data$group != i)
  
  # any data driven variable transformation or variable selection need to be added in each loop
  
  # define outcome and predictors
  Outcome <- "Group"
 
  CandidateVariables <- c("ratio4d","GCS","pneumonia","coagulopathy","age","neurosurgery4d","Ratio4d_1","Ratio4d_2") # create a formula
  Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                           paste(CandidateVariables, collapse=" + ")))
  
  # fit a model with all candidate varaibles
  
  
  model.full <- glm(Formula, data= data.train,family=binomial)
  
  # stepwise selection (as an example, other selection methods can be used as well)
  
  model.train <- stepAIC(model.full, direction="both")
  
  data.test <- subset(data,data$group == i)
  
  data.test$p_prediction <- predict(model.train, data.test, type="response")
  
  roc.test <- roc(data.test$Group, data.test$p_prediction)
  
  c_test[i] <- roc.test$auc
  
  brier_test[i] <- mean((data.test$p_prediction-data.test$Group)^2)
  
}

c_test
brier_test

summary(c_test)
summary(brier_test)






