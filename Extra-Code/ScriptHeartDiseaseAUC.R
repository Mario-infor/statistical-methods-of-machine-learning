library(MASS)
library(tidyverse)
library(ISLR2)
library(psych)
library(pROC)
library(vcd)
# libreria caret para crear particion de la muestra
library(caret)
library(ResourceSelection)
library(alluvial)
#install.packages("sparklyr")
library(sparklyr)
library(predict3d)
library(recipes)
library(rlang)
library(rpart)
library(rpart.plot)

df<-read.csv("framingham.csv", header=T)
head(df)
str(df)

df$male<- as.factor(df$male)
df$education<- as.factor(df$education)
df$currentSmoker<- as.factor(df$currentSmoker)
df$BPMeds<- as.factor(df$BPMeds)
df$prevalentStroke<- as.factor(df$prevalentStroke)
df$prevalentHyp<- as.factor(df$prevalentHyp)
df$diabetes<- as.factor(df$diabetes)
df$TenYearCHD<- as.factor(df$TenYearCHD)


df<-na.omit(df)

### make this example reproducible
set.seed(1)

### Training and validation sets
ind<- createDataPartition(df$TenYearCHD, p=0.8, list = F)
df.train<- df[ind,]
df.validation<- df[-ind,]

### fit logistic regression model
model <- glm(TenYearCHD~age+male+education+currentSmoker+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+totChol+sysBP+diaBP+BMI+heartRate+glucose, family="binomial", data=df.train)

### calculate probability of default for each individual in test dataset
predicted <- predict(model, df.validation, type="response")

### calculate AUC
library(pROC)
auc(df.validation$TenYearCHD, predicted)