library(ISLR2)
library(dplyr)
library(caret)
library(xgboost)

### Loading the database.
all <- read.csv("EleanyData.csv", stringsAsFactors = F)

### Converting categorical variables to factors.
all$sex<- as.factor(all$sex)
all$cp<- as.factor(all$cp)
all$fbs<- as.factor(all$fbs)
all$restecg<- as.factor(all$restecg)
all$exang<- as.factor(all$exang)
all$slope<- as.factor(all$slope)
all$thal<- as.factor(all$thal)
all$target<- as.factor(all$target)

boxplot(all$target ~ all$sex, xlab = "sex", ylab = "target")
boxplot(all$target ~ all$cp, xlab = "cp", ylab = "target")
boxplot(all$target ~ all$fbs, xlab = "fbs", ylab = "target")
boxplot(all$target ~ all$restecg, xlab = "restecg", ylab = "target")
boxplot(all$target ~ all$exang, xlab = "exang", ylab = "target")
boxplot(all$target ~ all$slope, xlab = "slope", ylab = "target")
boxplot(all$target ~ all$thal, xlab = "thal", ylab = "target")

### Chi-square test example
contingency_table <- table(all$sex, all$target)
result_chi2 <- chisq.test(contingency_table)

### Show the result
print(result_chi2)


### Using the randomForest method to find the variables with the highest importance.
quickRF <- randomForest(x=all[,-14], y=all$target, ntree=100,importance=TRUE)
impRF <- importance(quickRF)
impRF <- data.frame(Variables = row.names(impRF), MSE = impRF [,1])
impRF <- impRF [order(impRF$MSE, decreasing = TRUE),]

### Showing the graph of the 50 most important variables ordered from highest to lowest.
ggplot(impRF, aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")

all<- select(all,-trestbps, -chol, -restecg, -fbs)

train_index <- createDataPartition(all$target, times = 1, p = 0.8, list = FALSE)

train <- all[train_index ,]
test <- all[-train_index,]

#########################################################################################

boosting_model <- train(target~ ., data = train, method = 'xgbTree',
				trControl = trainControl(method = 'cv', number = 10,
								classProbs = TRUE,
								summaryFunction = twoClassSummary),
				metric = 'ROC')

boosting_model
summary(boosting_model)

varImp(boosting_model)
confusionMatrix(predict(boosting_model, test), test$target)

#########################################################################################

baggin_model <- train(target~ ., data = train, method = 'rf', importance = TRUE,
				trControl = trainControl(method = 'cv', number = 10))
baggin_model
summary(baggin_model)
varImp(baggin_model)
confusionMatrix(predict(baggin_model, test), test$target)

#########################################################################################

logistic_model <- train(target~ ., data = train, method = 'glm',
				trControl = trainControl(method = 'cv', number = 10))

logistic_model
summary(logistic_model)
varImp(logistic_model)
confusionMatrix(predict(logistic_model, test), test$target)



