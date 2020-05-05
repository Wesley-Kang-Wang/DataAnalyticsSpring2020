#read the data into R

EV.df <- read.csv("/Users/wesleywang/Documents/S20/ITWS 6600 Data Analytics/term project/Electric_Vehicle_Population_Data.csv")

str(EV.df) #see which deatures are factors and which ones are numeric

#Check the data set size of EV.df
dim(EV.df)

#Check the features available for interpretation
labels(EV.df)

#Dropo useless columns
EV.df <- subset(EV.df, select = -c(VIN..1.10.,X,Vehicle.Location,DOL.Vehicle.ID))
View(EV.df)

EV.df <- subset(EV.df,State=="WA") #get rid of entris that aren't in the state of WA
dim(EV.df) #verify that changes have been done to the dataset

EV.df <- subset(EV.df, select=-c(State))
View(EV.df)

#rename some columns
labels(EV.df)
colnames(EV.df) <- c("County","City","ZIP.Code","Model.Year","Make","Model", "Electric.Vehicle.Type",
                     "CFAV","Electric.Range","Base.MSRP","Legislative.District")
View(EV.df)

#EDA
sum(is.na(EV.df))
summary(EV.df)

hist(EV.df$Electric.Range, main="Electric Range Histogram",xlab="Electric Range",ylab="Count",col="green")
hist(EV.df$Base.MSRP, main="Base Model MSRP Histogram",xlab="Base MSRP",ylab="Count",col="green")

boxplot(EV.df$Electric.Range,main="Electric Range Distribution")
boxplot(EV.df$Base.MSRP,main="MSRP Base Price Distribution")

Model_Year <- as.data.frame(table(EV.df$Model.Year))
barplot(
  Model_Year$Freq,
  names.arg =Model_Year$Var1,
  main = "Model Years Count",
  xlab = "Year",
  ylab = "Frequency",
  col = "green",
  axes = TRUE,
  axisnames = TRUE
)

CFAV <- as.data.frame(table(EV.df$CFAV))
barplot(
  CFAV$Freq,
  names.arg =c("Eligible","Not Eligible"),
  main = "CFAV Count",
  xlab = "Eligibility",
  ylab = "Frequency",
  col = "green",
  axes = TRUE,
  axisnames = TRUE
)

Battery <- as.data.frame(table(EV.df$Electric.Vehicle.Type))
barplot(
  Battery$Freq,
  names.arg =c("BEV","PHEV"),
  main = "CFAV Count",
  xlab = "Eligibility",
  ylab = "Frequency",
  col = "green",
  axes = TRUE,
  axisnames = TRUE
)

EV.df[which.max(EV.df$Base.MSRP),] #Vehicle make and model that has the higheset price

#Logistic Regression
lm.fit<-glm(Base.MSRP~ZIP.Code,data=EV.df)
summary(lm.fit)

lm.fit2<-glm(Base.MSRP~Make,data=EV.df)
summary(lm.fit2)
mean(lm.fit2$coefficients)

lm.fit3<-glm(Base.MSRP~Electric.Range,data=EV.df)
summary(lm.fit3)

lm.fit4<-glm(Base.MSRP~County,data=EV.df)
summary(lm.fit4)
mean(lm.fit4$coefficients)

lm.fit5<-glm(Base.MSRP~CFAV,data=EV.df)
summary(lm.fit5)
mean(lm.fit5$coefficients)

lm.fit6<-glm(Base.MSRP~Electric.Vehicle.Type,data=EV.df)
summary(lm.fit6)

#NAs
sum(is.na(EV.df)) #455 Nulls
sum(is.na(EV.df$Base.MSRP)) #455 Nulls in Base.MSRP

#replace the NAs with the first quartile since the models with NAs are e-golf, which is at a relatively low price point
EV.df$Base.MSRP[is.na(EV.df$Base.MSRP)==TRUE] <- 31635
sum(is.na(EV.df))
mean(EV.df$Base.MSRP)
View(EV.df)

#Look at the levels of our categorical variables
str(EV.df)

#create dummy variables for categorizal features
#library(dummies)
#View(EV.df)
#EV.df.Dummy <- dummy.data.frame(EV.df,names=c("City","County","Make","Model","ZIP.Code","Electric.Vehicle.Type","CFAV","Legislative.District"), sep=".")
#View(EV.df.Dummy)


#Random Foprest Regression
library(dplyr)
EV.df.RF <- subset(EV.df,select=-c(County,City,Model))
str(EV.df.RF)

set.seed(00)
library(randomForest)
library(caret)

smp_size <- floor(0.8 * nrow(EV.df.RF))
train_ind <- sample(seq_len(nrow(EV.df.RF)), size = smp_size)
train <- EV.df.RF[train_ind, ]
test <- EV.df.RF[-train_ind, ]

rf <- randomForest(Base.MSRP~., train, ntree=150)
rf

rf_pred <- predict(rf, test[,!colnames(test) %in% c("Base.MSRP")])

imp <- importance(rf)
imp

varImp <- data.frame(Variables = row.names(imp))

rankImp <- varImp %>% mutate(Rank = paste0('#',dense_rank(desc(imp))))
ggplot(rankImp, aes(reorder(Variables, imp), imp, 
                    fill = imp)) +
  geom_bar(stat='identity') + 
  geom_text(aes(Variables, 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'yellow') +
  labs(x = 'Variables') +
  coord_flip()



d <- dist(, method = "euclidean")
fit <- hclust(d, method="ward.D") 
plot(fit)