library(ISLR2)
library(PerformanceAnalytics)
library(ggplot2)
library(magrittr)
library(interactions)
library(plyr)
library(dplyr)
library(leaps)
library(psych)
library(caret)
library(fuzzySim)
library(randomForest)
library(glmnet)

### Loading the database.
all <- read.csv("data.csv", stringsAsFactors = F)

###########################################################################################################
#################### Preparing the data #################################################################

### Checking how many columns with NA are there.
NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
cat('Hay', length(NAcol), 'columnas con valores NA')

### Preparing some help variables to be able to convert variables into 
### ordinals when the order of their values ​​is significant.
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)

### Changing the NA values ​​of some columns where the NA does not
### necessarily mean absence of data but rather means that the home
### does not have that quality.
all$Alley[is.na(all$Alley)] <- 'None'
all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$GarageType[is.na(all$GarageType)] <- 'No Garage'
all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$PoolQC[is.na(all$PoolQC)] <- 'None'
all$Fence[is.na(all$Fence)] <- 'None'
all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0
all$MasVnrArea[is.na(all$MasVnrArea)] <-0

### Eliminating the rest of the NA values where it does
### means absence of data.
all<- na.omit(all)
dim(all)

### Converting categorical variables into factors.
all$MSZoning<- as.factor(all$MSZoning)
all$Alley<- as.factor(all$Alley)
all$LandContour<- as.factor(all$LandContour)
all$Utilities<- as.factor(all$Utilities)
all$LotConfig<- as.factor(all$LotConfig)
all$Neighborhood<- as.factor(all$Neighborhood)
all$Condition1<- as.factor(all$Condition1)
all$Condition2<- as.factor(all$Condition2)
all$BldgType<- as.factor(all$BldgType)
all$HouseStyle<- as.factor(all$HouseStyle)
all$RoofStyle<- as.factor(all$RoofStyle)
all$RoofMatl<- as.factor(all$RoofMatl)
all$Exterior1st<- as.factor(all$Exterior1st)
all$Exterior2nd<- as.factor(all$Exterior2nd)
all$Foundation<- as.factor(all$Foundation)
all$Heating<- as.factor(all$Heating)
all$Electrical<- as.factor(all$Electrical)
all$GarageType<- as.factor(all$GarageType)
all$Fence<- as.factor(all$Fence)
all$MiscFeature<- as.factor(all$MiscFeature)
all$SaleType<- as.factor(all$SaleType)
all$SaleCondition<- as.factor(all$SaleCondition)

### These two variables are originally numerical but in fact 
### they should be categorical.
all$MoSold <- as.factor(all$MoSold)
all$MSSubClass <- as.factor(all$MSSubClass)

### Converting into ordinal those variables where the order 
### of their values ​​is significant.
all$PoolQC<-as.integer(revalue(all$PoolQC, Qualities))
all$FireplaceQu<-as.integer(revalue(all$FireplaceQu, Qualities))
all$LotShape<-as.integer(revalue(all$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
all$GarageFinish<-as.integer(revalue(all$GarageFinish, Finish))
all$GarageQual<-as.integer(revalue(all$GarageQual, Qualities))
all$GarageCond<-as.integer(revalue(all$GarageCond, Qualities))
all$BsmtQual<-as.integer(revalue(all$BsmtQual, Qualities))
all$BsmtCond<-as.integer(revalue(all$BsmtCond, Qualities))
all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, Exposure))
all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, FinType))
all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, FinType))
all$MasVnrType<-as.integer(revalue(all$MasVnrType, Masonry))
all$KitchenQual<-as.integer(revalue(all$KitchenQual, Qualities))
all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 
						'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
all$ExterQual<-as.integer(revalue(all$ExterQual, Qualities))
all$ExterCond<-as.integer(revalue(all$ExterCond, Qualities))
all$HeatingQC<-as.integer(revalue(all$HeatingQC, Qualities))
all$CentralAir<-as.integer(revalue(all$CentralAir, c('N'=0, 'Y'=1)))
all$LandSlope<-as.integer(revalue(all$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
all$Street<-as.integer(revalue(all$Street, c('Grvl'=0, 'Pave'=1)))
all$PavedDrive<-as.integer(revalue(all$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))

### Eliminating the ID and Utilities columns, ID does not provide information
### about the value of a home and Utilities has the same value in all its rows,
### so it does not provide useful information either.
all$Id<- NULL
all$Utilities<- NULL

### Creating some variables that group two or more columns into one given that their 
### values have some similarity or relationship.

### TotBathrooms: contains the sum of the bathrooms in the home.
all$TotBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)

### Remod: contains information about whether a home was remodeled or not.
all$Remod <- ifelse(all$YearBuilt==all$YearRemodAdd, 0, 1) #0=No remodeled, 1=Remodeled 

### Age: contains information about the years that passed from when the home
### was remodeled until it was sold.
all$Age <- as.numeric(all$YrSold)-all$YearRemodAdd

### IsNew: contains information about whether the home was sold in the same year it was built.
all$IsNew <- ifelse(all$YrSold==all$YearBuilt, 1, 0)

### TotalSqFeet: contains the sum of the square feet of the home.
all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF

### TotalSqFeet: contains the sum of all the square feet of the portal.
all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch

### NeighRich: groups the different neighborhoods into just three levels (poor=0, medium=1, rich=2)
all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale',
							 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

### Eliminating the variables that were previously grouped to avoid repeating information.
all$FullBath <- NULL
all$HalfBath <- NULL
all$BsmtFullBath <- NULL
all$BsmtHalfBath <- NULL
all$GrLivArea <- NULL
all$TotalBsmtSF<- NULL
all$OpenPorchSF <- NULL
all$EnclosedPorch<- NULL
all$X3SsnPorch <- NULL
all$ScreenPorch <- NULL
all$Neighborhood<- NULL

### Converting YrSold to factor.
all$YrSold <- as.factor(all$YrSold)

### Separating the data set into numerical and categorical to work on them separately.
numericVars <- which(sapply(all, is.numeric))
factorVars <- which(sapply(all, is.factor))
cat('Hay', length(numericVars), 'variables numéricas, y', length(factorVars), 'variables categóricas')

### Taking a list with the names of numerical variables.
numericVarNames <- names(numericVars)

### The corSelect method takes the pairs of variables that are correlated above a threshold and eliminates
### the one that has the least correlation with the response variable (SalePrice).
excludedVars <- corSelect(all, sp.cols = 68 ,numericVarNames [c(-45)] , cor.thresh = 0.7)

### selectedNumericVars contains the numeric variables that remain after applying the corSelect method.
selectedNumericVars <- select(all, excludedVars$selected.vars)

### Selecting only factor type variables.
factorVarsData <- select(all, all_of(factorVars))

### Creating dummy variables for all factor type variables.
DFdummies <- as.data.frame(model.matrix(~., factorVarsData))

### Eliminating dummy variables that have less than 10 values ​​equal to 1 in their columns
### because it is considered that they have very few appearances in the database and therefore
### do not provide much information.
fewOnes <- which(colSums(DFdummies[1:nrow(DFdummies),])<10)
DFdummies <- DFdummies[,-fewOnes]


###########################################################################################################
#################### Normalization of Numerical Data ###################################################

### Passing all numeric data to allNumeric variable.
allNumeric <- selectedNumericVars

### Applying the logarithm to all columns whose absolute value of the asymmetry
### coefficient is greater than 0.8.
for(i in 1:ncol(allNumeric)){
        if (abs(skew(allNumeric[,i]))>0.8){
                allNumeric[,i] <- log(allNumeric[,i] +1)
        }
}

### Centering and scaling all values.
PreNum <- preProcess(allNumeric, method=c("center", "scale"))

### Normalizing the data.
DFnorm <- predict(PreNum, allNumeric)

### Viewing the asymmetry coefficient of the response variables.
SalePrice <- all$SalePrice
skew(SalePrice)
qqnorm(SalePrice)
qqline(SalePrice)

### Applying the logarithm to the response variable because its asymmetry coefficient is too high.
SalePrice  <- log(SalePrice)
skew(SalePrice)
qqnorm(SalePrice)
qqline(SalePrice)

##############################################################################################################
#################### Select the most important variables ##############################################

### Joining the normalized numerical variables, the generated dummy variables and
### the response variable (SalePrice).
combined <- cbind(DFnorm, DFdummies)
all <- cbind(combined, SalePrice)

### Using the randomForest method to find the variables with the highest importance.
set.seed(2018)
quickRF <- randomForest(x=all[,-155], y=all$SalePrice, ntree=100,importance=TRUE)
impRF <- importance(quickRF)
impRF <- data.frame(Variables = row.names(impRF), MSE = impRF [,1])
impRF <- impRF [order(impRF$MSE, decreasing = TRUE),]

### Showing the graph of the 50 most important variables ordered from highest to lowest.
ggplot(impRF[1:50,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")

### Selecting the 30 most important variables of all.
importantVars <- row.names(impRF[1:30,])
all <- all[,c(importantVars, "SalePrice")]


##############################################################################################################
#################### Train-test validation approach with regsubsets and MSE ##############################################

### Creating the training (80%) and test (20%) sets.
index <- createDataPartition(all$SalePrice, times = 1, p = 0.8, list = FALSE)
train <- all[index ,]
test <- all[-index,]

### Using the regsubsets method to exhaustively explore all possible combinations
### up to 30 variables.
set.seed(1)
regfitBest <- regsubsets(train$SalePrice ~ ., data = train, nvmax = 30)
regSummary <- summary(regfitBest)
table <- as.data.frame(regSummary$outmat)
numberCols <- ncol(regSummary$outmat)
shortNames <- paste0("C_", 1:numberCols) 
colnames(table) <- shortNames
table

### Calculating the MSE for each model. 
testMatrix <- model.matrix(test$SalePrice~ ., data = test)
val.errors <- rep(NA, 30)
for (i in 1:30) {
 coefi <- coef(regfitBest, id = i)
 pred <- testMatrix[, names(coefi)] %*% coefi
 val.errors[i] <- mean((exp(test$SalePrice) - exp(pred))^2) ### The exp() function is applied 
}										### to view the MSE without normalization.

### Selecting the model with the lowest MSE.
val.errors
which.min(val.errors)

### Showing the coefficients of the model with the smallest MSE.
coef(regfitBest, which.min(val.errors))

##############################################################################################################
######### CrossValidation approach with regsubsets and MSE ########################################################
 
### Creating a custom function to predict with regsubset type objects since
### you cannot use R's predict function directly.
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

### Setting the number of folds (k) and the number of rows (n).
k <- 10
n <- nrow(all)

### Calculating the MSE for each of the models.
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 30, dimnames = list(NULL, paste(1:30)))
for (j in 1:k) {
  best.fit <- regsubsets(SalePrice ~ .,
       data = all[folds != j,],
       nvmax = 30)
  for (i in 1:30) {
    pred <- predict(best.fit, all[folds == j, ], id = i)
    cv.errors[j, i] <- mean((exp(all$SalePrice[folds == j]) - exp(pred))^2)  ### The exp() function is applied
												     ### to view the MSE without normalization.
   }
}

### Calculating the average of the MSE obtained in the folds of each model. 
mean.cv.errors <- apply(cv.errors, 2, mean)

### Choosing the best model according to the MSE.
which.min(mean.cv.errors)
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")

### Shown the coefficients of the model with the lowest MSE.
reg.best <- regsubsets(SalePrice ~ ., data = all, nvmax = 18)
coef(reg.best, which.min(mean.cv.errors))

##############################################################################################################
######### CrossValidation Approach with Ridge Regression and Lasso ###############################################
 
### Separating the data set into training (80%) and testing (20%).
index <- createDataPartition(all$SalePrice, times = 1, p = 0.8, list = FALSE)
train <- all[index ,]
test <- all[-index,]

### model.matrix builds a database by decomposing the categories in terms of
### the necessary dichotomies.
x <- model.matrix(all$SalePrice ~ ., all)[, -1]
y <- all$SalePrice
xTrain <- model.matrix(SalePrice ~ ., train)[, -1]
yTrain <- train$SalePrice
xTest <- model.matrix(SalePrice ~ ., test)[, -1]
yTest <- test$SalePrice

############################ Ridge ######################################################

### Creating 100 values ​​between 10^10 and 10^-2 for lambda values ​​(penalty parameter).
grid <- 10^seq(10, -2, length = 100)

### Trying to create models with all lambda values ​​until the change in coefficients is less
### than the value specified in thresh.
set.seed(1)
ridgeMod <- cv.glmnet(xTrain, yTrain, alpha = 0, lambda = grid, thresh = 1e-12, nfolds = 10)
plot(ridgeMod)

### Finding the optimal lambda value.
bestLambdaRidge <- ridgeMod$lambda.min

### Finding the Ridge model using the optimal value of lambda.
finalModelRidge <- glmnet(xTrain, yTrain, alpha = 0, lambda = bestLambdaRidge)

### Calculating the MSE value for the Ridge model.
ridgePred <- predict(finalModelRidge , s = bestLambdaRidge, newx = xTest)
mean((exp(ridgePred) - exp(yTest))^2) 	### The exp() function is applied
							### to view the MSE without normalization.

### Showing the model coefficients.
predict(finalModelRidge, type = "coefficients", s = bestLambdaRidge)

############################ Lasso #######################################################

### Creating 100 values ​​between 10^10 and 10^-2 for lambda values ​​(penalty parameter).
grid <- 10^seq(10, -2, length = 100)

### Trying to create models with all lambda values ​​until the change in coefficients is 
### less than the value specified in thresh.
set.seed(1)
lassoMod <- cv.glmnet(xTrain, yTrain, alpha = 1, lambda = grid, thresh = 1e-12, nfolds = 10)
plot(lassoMod)

### Finding the optimal lambda value.
bestLambdaLasso <- lassoMod$lambda.min

### Finding the Lasso model using the optimal value of lambda.
finalModel <- glmnet(xTrain, yTrain, alpha = 1, lambda = bestLambdaLasso)

### Calculating the MSE value for the Lasso model.
lassoPred <- predict(lassoMod, s = bestLambdaLasso, newx = xTest)
mean((exp(lassoPred) - exp(yTest))^2)	### The exp() function is applied
							### to view the MSE without normalization.

### Showing the model coefficients.
predict(finalModel, type = "coefficients", s = bestLambdaLasso)


##############################################################################################################
################## EXTRA  EXTRA  EXTRA  EXTRA  EXTRA  EXTRA ##################################################
##############################################################################################################
######### CrossValidation approach with regsubsets and adjusted R-squared ########################################################

### Setting the number of folds (k) and the number of rows (n).
k <- 10
n <- nrow(all)

### Calculating the adjusted R squared for each of the models.
set.seed(1)
folds <- sample(rep(1:k, length = n))
cvAdjustedRSquared <- matrix(NA, k, 30, dimnames = list(NULL, paste(1:30)))
for (j in 1:k) {
  best.fit <- regsubsets(SalePrice ~ .,
       data = all[folds != j,],
       nvmax = 30)
  summaryBestFit <- summary(best.fit)
  cvAdjustedRSquared[j,] <- summaryModel$adjr2
}

### Calculating the average of the adjusted R square obtained in the folds of each model.
meanCvAdjustedRSquared  <- apply(cvAdjustedRSquared, 2, mean)

### Choosing the best model according to the adjusted R square.
which.max(meanCvAdjustedRSquared )
par(mfrow = c(1, 1))
plot(meanCvAdjustedRSquared , type = "b")

### Shown the coefficients of the model with the largest adjusted R squared.
reg.best <- regsubsets(SalePrice ~ ., data = all, nvmax = 30)
coef(reg.best, which.max(meanCvAdjustedRSquared))



