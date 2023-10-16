library(MASS)
library(ISLR2)
library("PerformanceAnalytics")
library(ggplot2)
library(magrittr)
library(interactions)
library(dplyr)
library(Metrics)
library(caret)

dataHour<-read.csv("hour.csv", header=T)
dataHourReduced<- select(dataHour, -instant, -dteday)
dataHourReduced<-na.omit(dataHourReduced)

attach(dataHourReduced)
head(dataHourReduced)
str(dataHourReduced)

dataHourReduced$season<- as.factor(dataHourReduced$season)
dataHourReduced$yr<- as.factor(dataHourReduced$yr)
dataHourReduced$mnth<- as.factor(dataHourReduced$mnth)
dataHourReduced$hr<- as.factor(dataHourReduced$hr)
dataHourReduced$holiday<- as.factor(dataHourReduced$holiday)
dataHourReduced$weekday<- as.factor(dataHourReduced$weekday)
dataHourReduced$workingday<- as.factor(dataHourReduced$workingday)
dataHourReduced$weathersit<- as.factor(dataHourReduced$weathersit)

boxplot(dataHourReduced$cnt ~ dataHourReduced$season, xlab = "season", ylab = "cnt")
boxplot(dataHourReduced$cnt ~ dataHourReduced$yr, xlab = "yr", ylab = "cnt")
boxplot(dataHourReduced$cnt ~ dataHourReduced$mnth, xlab = "mnth", ylab = "cnt")
boxplot(dataHourReduced$cnt ~ dataHourReduced$hr, xlab = "hr", ylab = "cnt")
boxplot(dataHourReduced$cnt ~ dataHourReduced$holiday, xlab = "holiday", ylab = "cnt")
boxplot(dataHourReduced$cnt ~ dataHourReduced$weekday, xlab = "weekday", ylab = "cnt")
boxplot(dataHourReduced$cnt ~ dataHourReduced$workingday, xlab = "workingday", ylab = "cnt")
boxplot(dataHourReduced$cnt ~ dataHourReduced$weathersit, xlab = "weathersit", ylab = "cnt")

chart.Correlation(select(dataHourReduced, temp, atemp, hum, windspeed, cnt), histogram=TRUE, pch=19)

###########################################################################################################
#############################Building the model########################################################

min.model <- lm(cnt~1, data= dataHourReduced)
summary(min.model)

fwd.model <- step(min.model, direction="forward",scope=(~season+yr+mnth+hr+holiday+weekday+
	workingday+weathersit+temp+atemp+hum+windspeed)) 
summary(fwd.model)

fwd.model <- step(min.model, direction="forward",scope=(~hr+atemp+yr+weathersit+season+mnth+
	hum+weekday+workingday+windspeed+temp))
summary(fwd.model)
fwd.model$coefficients

prediccion<-fitted.values(fwd.model)
mse(cnt, prediccion)

##########################################################################################################
#############################Model validation using k-folds#######################################

folds <- createFolds(dataHourReduced$cnt, k = 10)
ctrl <- trainControl(method = "cv", index = folds)
model <- train(cnt ~ hr+atemp+yr+weathersit+season+mnth+hum+weekday+workingday+windspeed+temp,
 	data = dataHourReduced, method = "lm", trControl = ctrl)

model$resample
print(model)

##########################################################################################################
#############################Validación del modelo mediante Split train-test##############################

training_index <- sample(nrow(dataHourReduced), round(nrow(dataHourReduced)*0.8), replace = FALSE)
training_data <- dataHourReduced[training_index, ]
test_data <- dataHourReduced[-training_index, ]

modelo <- lm(cnt ~ hr+atemp+yr+weathersit+season+mnth+hum+weekday+workingday+windspeed+temp, data = training_data)

predictions <- predict(modelo, newdata = test_data)
MSE <- mean((predictions - test_data$cnt)^2)
R2 <- summary(modelo)$r.squared
MSE 
R2 