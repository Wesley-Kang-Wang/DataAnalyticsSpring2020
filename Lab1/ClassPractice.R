days <- c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')  #days
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4) #temperature in F 
snowed <- c('T','T','F','F','T','T','F')

RPI_Weather_Week <- data.frame(days,temp,snowed)

RPI_Weather_Week

head(RPI_Weather_Week) #show the first 6 rows default

str(RPI_Weather_Week) #show the structure of the dataframe

summary(RPI_Weather_Week) #show a summary of the dataframe according to the differnet data types

RPI_Weather_Week[1,] #select only the first row of the dataframe

RPI_Weather_Week[,1] #select only the first column of the dataframe

RPI_Weather_Week[,'days'] #call columns using column name

RPI_Weather_Week[1:5,c('days','temp')] #call range of rows and columns using vector

RPI_Weather_Week$temp #using $ to call a column

subset(RPI_Weather_Week,subset=snowed=='T') 

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow <- order(RPI_Weather_Week$temp) 
dec.snow                                  #descending order based on temperature

#######################################################################################

empty.Dataframe <- data.frame() #create an empty dataframe

v1 <- 1:10
v1

letters
v2<-letters[1:10]

df <- data.frame(col.name.1 = v1, col.name.2=v2)
df

write.csv(df, file='saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2 

####################################################################################

EPI <- read.csv(file.choose(),header=T,skip=1)
EPI

GPW <- read.csv(file.choose(),header=T)
GPW

boxplot(GPW$NumUnits) #create box plot for num of units in GPW
boxplot(EPI$ECOSYSTEM) #create box plot for ecosystem in EPI

############################################################################
fix(EPI)
tf<-is.na(EPI$EPI)
E<-EPI$EPI[!tf]

summary(EPI$EPI)

fivenum(EPI$EPI,na.rm=TRUE)

stem(EPI$EPI)
hist(EPI$EPI)

hist(EPI$EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI$EPI,na.rm=TRUE,bw='SJ'))
rug(EPI$EPI)


plot(ecdf(EPI$EPI), do.points=FALSE, verticals=TRUE) #plot the cumulative density function

par(pty="s")
qqnorm(EPI$EPI); qqline(EPI$EPI)  #Q-Q plot for the EPI dataset

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t
dsn")
qqline(x)



#Exercise 1

summary(EPI$DALY)
hist(EPI$DALY)
boxplot(EPI$EPI,EPI$DALY)

summary(EPI$BIODIVERSITY)
hist(EPI$BIODIVERSITY)
boxplot(EPI$EPI,EPI$BIODIVERSITY)

plot(ecdf(EPI$DALY), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPI$DALY); qqline(EPI$DALY)

plot(ecdf(EPI$BIODIVERSITY), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPI$BIODIVERSITY); qqline(EPI$BIODIVERSITY)

#Exercise 2
attach(EPI)
EPILand <- EPI$EPI[!EPI$Landlock]
Eland <- EPILand[!is.na(EPILand)]

hist(Eland)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

#filter on EPI_regions
EPI_nonEurope <- EPI$EPI[EPI$EPI_regions!='Europe']
hist(EPI_nonEurope)

EPI_SouthAsia <- EPI$EPI[EPI$EPI_Regions=='South Asia']
EPI_SouthAsia


