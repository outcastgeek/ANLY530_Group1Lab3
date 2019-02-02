## @knitr installLibraries

install.packages("knitr")
install.packages("kableExtra")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cluster")
install.packages("rpart")
install.packages("rpart.plot") 
install.packages("RColorBrewer") 
install.packages("rattle")
install.packages("NbClust")

## @knitr loadLibraries

library(dplyr)
library(ggplot2)
library(cluster)
library(rpart)
library(rpart.plot) 
library(RColorBrewer) 
library(rattle)
library(NbClust)

## @knitr helperFunctions

# Obtains the full File Path
fullFilePath <- function(fileName)
{
  fileFolder <- "./data/"
  fileNamePath <- paste(fileFolder, fileName, sep = "")
  fileNamePath
}

# Converts column of Timestamps to Date
ttColToDate <- function(dFrame, colName) {
  dFrame[colName] <- as.POSIXct(dFrame[colName], origin="1970-01-01")
  dFrame
}

# Converts column to utf-8
toUtf8 <- function(column) {
  columnUtf8 <- iconv(enc2utf8(column), sub = "byte")
  columnUtf8
}

# Formats Data
fmt <- function(dt, caption = "") {
  fmt_dt <- dt %>%
    kable("latex", longtable = T, booktabs = T)
  fmt_dt
}

# Style Data
style <- function(dt, full_width = F, angle = 0) {
  style_dt <- dt %>%
    kable_styling(latex_options = "striped", full_width = full_width) %>%
    row_spec(0, angle = angle)
  style_dt
}

# Top Customers
top.n.custs <- function (data,cols,n=5) {  
  
  #Initialize a vector to hold customers being removed 
  idx.to.remove <-integer(0)  
  
  for (c in cols){  
    
    # For every column in the data we passed to this function 
    #Sort column "c" in descending order (bigger on top) 
    #Order returns the sorted index (e.g. row 15, 3, 7, 1, ...) rather than the actual values sorted.     
    
    col.order <-order(data[,c],decreasing=T)  
    
    
    #Take the first n of the sorted column C to 
    #combine and de-duplicate the row ids that need to be removed 
    
    idx <-head(col.order, n) 
    idx.to.remove <-union(idx.to.remove,idx) 
    
  } 
  
  #Return the indexes of customers to be removed 
  return(idx.to.remove)  
  
}

#Plot the within (cluster) sum of squares to determine the initial value for "k" 
wssplot <- function(data, nc=15, seed=1234){ 
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){ 
    set.seed(seed) 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)} 
  plot(1:nc, wss, type="b", xlab="Number of Clusters", 
       ylab="Within groups sum of squares")}

## @knitr loadSheets

#Set Data File Name:
Wholesale_customers_file <- "Wholesale_customers_data.csv"
wine_file <- "wine.csv"

# Wholesale Customers
Wholesale_customers_data <- Wholesale_customers_file %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)

# Wine
wine_data <- wine_file %>%
  fullFilePath %>%
  read.csv(encoding = "UTF-8", header=TRUE, stringsAsFactors=FALSE)


## @knitr part1Customers

#How Many Customers to be Removed? 
top.custs <-top.n.custs(Wholesale_customers_data, cols = 1:5,n=5) 
length(top.custs)

#Examine the customers 
Wholesale_customers_data[top.custs,]

#Remove the Customers 
Wholesale_customers_data.rm.top<-Wholesale_customers_data[-c(top.custs),]

#Examine summary stats for the remaining data
print(summary(Wholesale_customers_data.rm.top))

#Set the seed for reproducibility 
set.seed(76964057)

#Try K from 2 to 20 
rng<-2:20  

#Number of times to run the K Means algorithm 
tries <-100  

#Set up an empty vector to hold all of points 
avg.totw.ss <-integer(length(rng))  
avg.totb.ss <- integer(length(rng)) 
avg.tot.ss <- integer(length(rng)) 

# For each value of the range variable 
for(v in rng){  
  
  #Set up an empty vectors to hold the tries 
  v.totw.ss <-integer(tries)  
  b.totb.ss <- integer(tries) 
  tot.ss <- integer(tries) 
  
  #Run kmeans 
  for(i in 1:tries){ 
    k.temp <-kmeans(Wholesale_customers_data.rm.top,centers=v)  
    
    #Store the total withinss 
    v.totw.ss[i] <-k.temp$tot.withinss 
    
    #Store the betweenss 
    b.totb.ss[i] <- k.temp$betweenss 
    
    #Store the total sum of squares 
    tot.ss[i] <- k.temp$totss 
  } 
  
  #Average the withinss and betweenss 
  avg.totw.ss[v-1] <-mean(v.totw.ss) 
  avg.totb.ss[v-1] <-mean(b.totb.ss) 
  avg.tot.ss[v-1] <- mean(tot.ss) 
}

plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K", 
     ylab="Average Total Within Sum of Squares", 
     xlab="Value of K")

plot(rng,avg.totb.ss,type="b", main="Total between SS by Various K", 
     ylab="Average Total Between Sum of Squares", 
     xlab="Value of K")

#Plot the ratio of betweenss/total ss and withinss / total ss for evaluation 
plot(rng,avg.totb.ss/avg.tot.ss,type="b", main="Ratio of between ss / the total ss by Various K", 
     ylab="Ratio Between SS / Total SS", 
     xlab="Value of K") 
abline(h=0.85, col="red")

plot(rng,avg.totw.ss/avg.tot.ss,type="b", main="Ratio of within ss / the total ss by Various K", 
     ylab="Ratio Between SS / Total SS", 
     xlab="Value of K") 
abline(h=0.15, col="red")

#Create the best number of clusters, Remove columns 1 and 2 
#n <- readline(prompt = "Enter the best number of clusters: ")
#return(as.integer(n))

# pick n = 20
n = 20

k <-kmeans(Wholesale_customers_data.rm.top[,-c(1,2)], centers=n)

#Display&nbsp;cluster centers 
print(k$centers)

#Give a count of data points in each cluster 
print(table(k$cluster))

#Generate a plot of the clusters
clusplot(Wholesale_customers_data.rm.top, k$cluster, main='2D representation of the Cluster solution', 
         color=TRUE, shade=TRUE, 
         labels=2, lines=0)

## @knitr part2Wine

wine_df <- scale(wine_data[-1])

#Examine the data frame and plot the within sum of squares 
head(wine_df)
wssplot(wine_df)

#Start the k-Means analysis using the variable "nc" for the number of clusters 

set.seed(1234) 
nc <- NbClust(wine_df, min.nc=2, max.nc = 15, method = "kmeans") 

print(table(nc$Best.n[1,])) 

barplot(table(nc$Best.n[1,]), xlab = "Number of Clusters", ylab = "Number of Criteria", main = "Number of Clusters Chosen by 26 Criteria") 

#Enter the best number of clusters based on the information in the table and barplot 

#n <- readline(prompt = "Enter the best number of clusters:  ") 
#n <- as.integer(n) 

# pick n = 14
#n = 14

#Conduct the k-Means analysis using the best number of clusters 

#set.seed(1234)
#fit.km <- kmeans(wine_df, n, nstart=25) 

#print(fit.km$size) 

#print(fit.km$centers) 

#print(aggregate(wine_data[-1], by=list(cluster=fit.km$cluster), mean))

#Use a confusion or truth table to evaluate how well the k-Means analysis performed 
#ct.km <- table(wine_df$Type, fit.km$cluster) 
#print(ct.km)
