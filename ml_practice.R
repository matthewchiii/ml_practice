library(tidyverse)
#Linear Regression Model
library(mlbench)
library(caret)
#Load dataset (Value of owner-occupied homes given various conditions)
data("BostonHousing")
head(BostonHousing)
#Check for missing data
sum(is.na(BostonHousing))
#Set random seed number to achieve reproducible model
set.seed(100)
#Random split of dataset (80:20)
TrainingIndex <- createDataPartition(BostonHousing$medv, p = 0.8, list = FALSE)
TrainingSet <- BostonHousing[TrainingIndex,] #Training Set (80%), [x,] pulls rows x, [,x] pulls columns x
TestingSet <- BostonHousing[-TrainingIndex,] #Test Set (20%), all rows not x
#Building Training Model
Model <- train(medv ~ ., data = TrainingSet,
               method = 'lm',
               na.action = na.omit,
               preProcess = c('scale', 'center'),
               trControl = trainControl(method = 'none'))
#Apply model for prediction
Model.training <- predict(Model, TrainingSet) #Apply model to make prediction on training set
Model.testing <- predict(Model, TestingSet) #Apply model to make prediction on testing set
#Model performance
plot(TrainingSet$medv, Model.training, col = 'blue')
plot(TestingSet$medv, Model.testing, col = 'red')
summary(Model) #Estimate = how much each condition affects outcome (medv)
#Calculate Pearson's correlation coefficient
R.training <- cor(TrainingSet$medv, Model.training)
R.testing <- cor(TestingSet$medv, Model.testing)
R2.training <- R.training^2
R2.testing <- R.testing^2
