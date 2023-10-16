library(dplyr)
library(caret)
library(pROC)


set.seed(1)

dataEchocardiogram<-read.csv("Echocardiogram.csv", header=T)

dataEchocardiogramReduced<- select(dataEchocardiogram,-Name,
						-Group,-Mult,-Wall.motion.score,
						-Survival,-Still.alive)

dataReady<-na.omit(dataEchocardiogramReduced)

dataReady$Pericardial.effusion<- factor(dataReady$Pericardial.effusion)
dataReady$Alive.at.1<- factor(dataReady$Alive.at.1)

dataReady$Alive.at.1 <- factor(dataReady$Alive.at.1, labels = c('x0', 'x1'))

str(dataReady)
attach(dataReady)

train_index <- createDataPartition(dataReady$Alive.at.1, times = 1, p = 0.8, list = FALSE)

train <- dataReady[train_index ,]
test <- dataReady[-train_index,]

#################################################################################################################
########################## BOOSTING #############################################################################

boosting_model <- train(Alive.at.1~ ., data = train, method = 'xgbTree',
				trControl = trainControl(method = 'cv', number = 10,
								classProbs = TRUE,
								summaryFunction = twoClassSummary),
				metric = 'ROC')

boosting_model

varImp(boosting_model)
confusionMatrix(predict(boosting_model, test), test$Alive.at.1, positive = 'x1')

boosting_roc <- roc(response = test$Alive.at.1, predictor = predict(boosting_model, newdata = test,
										type = 'prob')$x1)

plot(boosting_roc, legacy.axes = TRUE)
auc(boosting_roc)

#################################################################################################################
########################## BAGGING ##############################################################################

baggin_model <- train(Alive.at.1~ ., data = train, method = 'rf', importance = TRUE,
				trControl = trainControl(method = 'cv', number = 10))
baggin_model

varImp(baggin_model)
confusionMatrix(predict(baggin_model, test), test$Alive.at.1, positive = 'x1')

bagging_roc <- roc(response = test$Alive.at.1, predictor = predict(baggin_model, newdata = test,
										type = 'prob')$x1)

plot(bagging_roc, legacy.axes = TRUE)
auc(bagging_roc)

#################################################################################################################
########################## LOGISTIC #############################################################################

logistic_model <- train(Alive.at.1~ ., data = train, method = 'glm',
				trControl = trainControl(method = 'cv', number = 10))

logistic_model

varImp(logistic_model)
confusionMatrix(predict(logistic_model, test), test$Alive.at.1, positive = 'x1')

logistic_roc <- roc(response = test$Alive.at.1, predictor = predict(logistic_model, newdata = test,
										type = 'prob')$x1)

plot(logistic_roc, legacy.axes = TRUE)
auc(logistic_roc)
