install.packages("randomForest")
library(randomForest)

#loading the dataset
data1 <- read.csv(file.choose(),header=TRUE)
head(data1)

#adding the column names
colnames(data1) <- c("BuyingPrice",'Maintenance','NumDoors','NumPersons','BootSpace','Safety','Condition')
head(data1)
str(data1)

#check the levels of the columns condition
levels(data1$Condition)
summary(data1)

##Creating the training and validation dataset
#we will randomly select 70% of the data points for training and 30% for validation
set.seed(100)
train <- sample(nrow(data1),0.7*nrow(data1),replace=FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

help("randomForest")

#random forest model with default parameters
model1 <- randomForest(Condition~.,data=TrainSet,importance=TRUE)
model1
#By default, number of trees is 500 and number of variables tried at each split is 2 in this case

##Fine tune the parameters of the random forest model
#we have increased the mtry to 6 from 2
#mtry = Number of variables randomly samples as candidates at each split
#Note that the default values are different for 
model2 <- randomForest(Condition~.,data=TrainSet,ntree=500,mtry=6,importance=TRUE)
model2

#First we will conduct prediction using training set after that we will do prediction using validation set. 
#Predicting on the training dataset
predTrain <- predict(model2,TrainSet,type='class')
#we can use the table() to check the classification accuracy
table(predTrain,TrainSet$Condition)
#Predicting on Validation set
predValid <- predict(model2,ValidSet,type='class')
table(predValid,ValidSet$Condition)

#we can also use importance() function to check important variables
#The below functions show the drop in mean accuracy or each of the variables
#To check the important variables
importance(model2)
varImpPlot(model2)

#Now, we will use 'for' loop and check for different values of mtry
#using a for loop to identify the right mtry for the model
a=c()
i=5
for (i in 3:8){
  model3 <- randomForest(Condition~.,data=TrainSet,ntree=500,mtry=i,)
  predValid <- predict(model3,ValidSet,type='class')
  a[i-2]=mean(predValid== ValidSet$Condition)
}
a
plot(3:8,a)

#Now, we have seen the implementation of Random Forest and understand the importance of the model.
#Let's compare this model with decision tree and see how decision trees fare in comparison to random forest.
#Compare with Decision Tree
library(rpart)
library(caret)
library(e1071)

#We will compare model1 of Random Forest with Decision Tree Model
model_dt <- train(Condition~.,data=TrainSet,method='rpart')
model_dt_1 <- predict(model_dt,data=TrainSet)

table(model_dt_1,TrainSet$Condition)
mean(model_dt_1==TrainSet$Condition)
table(model_dt_1,TrainSet$Condition)
mean(model_dt_1==TrainSet$Condition)


#On the training dataset,the accuracy is around 79.4% and there is lot of misclassification.
#Now, look at the validation dataset.

#Running on Validation set.
model_dt_vs = predict(model_dt,newdata=ValidSet)
table(model_dt_vs,ValidSet$Condition)
mean(model_dt_vs == ValidSet$Condition)

