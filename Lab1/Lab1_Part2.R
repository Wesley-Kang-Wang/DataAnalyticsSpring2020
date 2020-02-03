install.packages('dplyr')
library(dplyr)
multivariate <-read.csv("multivariate.csv")
attach(multivariate)

#Multivariate Regression
head(multivariate)
help(lm)
mm <- lm(Homeowners~Immigrant)
mm

summary(mm)$coef

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)

newimmigrantdata <- data.frame(Immigrant=c(0,20))
mm %>% predict(newimmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

#Creating Plots
#Chapter 2 -- R Graphics Cookbook
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type='l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col='red')
points(pressure$temperature,pressure$pressure/2,col='blue')

qplot(pressure$temperature,pressure$pressure,geom='line')
qplot(temperature,pressure,data=pressure,geom='line')

ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

#creating bar graphs
barplot(BOD$deman,names.arg=BOD$TIME)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generate a table of counts
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

#Creating histogram
#view the distribution of one-dimensional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg,breaks=10)
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12) #specify approximate number of bins with breaks
qplot(mpg,data=mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=5)

#creating Box-plot
plot(ToothGrowth$supp,ToothGrowth$len)
#formula syntax
boxplot(len ~ supp,data=ToothGrowth)

#put interaction of two variables on x-axis
boxplot(len~supp+dose,data=ToothGrowth)
#with ggplot2 to get the same results
qplot(ToothGrowth$supp,ToothGrowth$len,geom='boxplot')
qplot(supp,len,data=ToothGrowth,geom='boxplot')
qplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
#using three separate vectors
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom='boxplot')
#to get the same thing above using data frame
qplot(interaction(supp,dose),len,data=ToothGrowth,geom='boxplot')
#do the same thing using ggplot
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()
