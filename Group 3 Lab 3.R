#load the wine data from the website
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
#take a look at the first few rows of the dataset
head(wine_data)

#take a look at the number of rows and dimensions
nrow(wine_data)
dim(wine_data)

#adding the variable names
colnames(wine_data) <- c("Cvs","Alcohol","Malic_Acid","Ash","Alkalinity_of_Ash","Magnesium","Total_Phenols","Flavaoids","NonFlavanoid_Phenols","Proanthocyanins","Color_Intensity","Hue","0D280/0D315_of_Diluted_Wine","Proline")
head(wine_data)

#show the correlation using heat map
heatmap(cor(wine_data),Rowv=NA,Colv=NA)

#Our goal is to identify the 3 variates based on the chemical data on the wine dataset.
#In order to make it easy to identify the 3 cultivars, we will declare 3 classes that represents 
#each cultivar (Cv1,Cv2,Cv3,Cv4) by using the factore() function 

#declaring the cultivar_classes using the factor()function each cultivar 
cultivar_classes <- factor(wine_data$Cvs)
cultivar_classes

#We wil normalize the wine data to a common scale using scale() function so that the PCA 
#process will not overweight variables that happen to have the longer values

#We will not normalize the Cvs variable sowe exclude the column with -1
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)

