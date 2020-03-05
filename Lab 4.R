set.seed(12345)
help(par)

par(mar=rep(0.2,4))
data_Matrix <- matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])

par(mar=rep(0.2,4))
heatmap(data_Matrix)

#random coin flip
set.seed(678910)


for(i in 1:40){
  #flip a coin and get the data
  coin_Flip <- rbinom(1,size=1,prob=0.5)
  #if the coin is "Heads", add a common pattern to that row
  if(coin_Flip){
    data_Matrix[i,] <- data_Matrix[i,]+rep(c(0,3),each=5)
  }
}

par(mar=rep(0.2,4))
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])

par(mar=rep(0.2,4))
heatmap(data_Matrix)

#take a closer look at the patterns in rows and columns by looking at the marginal means of the rows and columns
hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1,xlab="The Row Mean",ylab="Row",pch=19)
plot(colMeans(data_Matrix_Ordered),xlab='Column',ylab='Column Mean',pch=19)

#left plot has the original data recorded
#Middle plot has the mean of the each rows (40 rows thus 40 dots representing each row)
#right plot has 10 dots for each of the 10 columns

