library(rpart)
library(rpart.plot)
library(ggplot2)

data("msleep")
str(msleep)
help("msleep")
str(data)

#create a new data frame with some columns included
mSleepDF1 <- msleep[,c(3,6,10,11)]
str(mSleepDF1)
head(mSleepDF1)

#read the documentation for the rpart() function
help(rpart)
#build the model
sleepModel_1 <- rpart(sleep_total~.,data=mSleepDF1,method='anova')
sleepModel_1

#visualize the results
rpart.plot(sleepModel_1,type=3,fallen.leaves=TRUE)
rpart.plot(sleepModel_1,type=3,digits=3,fallen.leaves = TRUE)
rpart.plot(sleepModel_1,type=3,digits=4,fallen.leaves = TRUE)

#classification tree
install.packages("C50")
require(C50)

data("iris")
head(iris)
str(iris)
table(iris$Species)

#set the seeds
set.seed(9850)
#generate random numbers
grn <- runif(nrow(iris))

#create a randomized iris dataset
irisrand <- iris[order(grn),]

#observe the rows that were randomly generated
str(irisrand)
classificationmodel1 <- C5.0(irisrand[1:100,-5],irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)

#prediction
prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1

#confusion matrix
table(irisrand[101:150,5],prediction1)

#plot the ctree
plot(classificationmodel1)

#naive bayes lassifier package e1071
library("e1071")
classifier <- naiveBayes(iris[,1:4],iris[,5])
table(predict(classifier,iris[,5]),iris[,5],dnn=list('predicted','actual'))

classifier$apriori
classifier$tables$Petal.Length

plot(function(x)dnorm(x,1.462,0.1736640),0,8,col="red",main="Petal Length distribution for the 3 different species")
curve(dnorm(x,4.260,0.4699110),add=TRUE,col='blue')
curve(dnorm(x,5.552,0.5518947),add=TRUE,col='green')

