library(MASS)
library(ISLR2)
library("PerformanceAnalytics")
library(ggplot2)
library(magrittr)
library(interactions)
library(dplyr)
library(Metrics)


datosHour<-read.csv("hour.csv", header=T)
datosHourReduced<- select(datosHour, -instant, -dteday, -casual, -registered)
head(datosHourReduced)
str(datosHourReduced)
attach(datosHourReduced)
chart.Correlation(select(datosHourReduced, season, yr, mnth, hr, cnt), histogram=TRUE, pch=19)
chart.Correlation(select(datosHourReduced, holiday, weekday, workingday, weathersit, cnt), histogram=TRUE, pch=19)
chart.Correlation(select(datosHourReduced, temp, atemp, hum, windspeed, cnt), histogram=TRUE, pch=19)

min.model <- lm(cnt~1, data= datosHourReduced)
fwd.model <- step(min.model, direction="forward",scope=(~season+yr+mnth+hr+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed)) 
summary(fwd.model)

fwd.model <- step(min.model, direction="forward",scope=(~season+yr+mnth+hr+weekday+atemp+hum+windspeed)) 
summary(fwd.model)
fwd.model$coefficients

prediccion<-fitted.values(fwd.model)
mse(cnt, prediccion)
