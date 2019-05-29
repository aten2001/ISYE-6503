#Question 4.1
Describe a situation or problem from your job, everyday life, current events, etc., for which a clustering
model would be appropriate. List some (up to 5) predictors that you might use.

a university may want to find out the pattern of the alumni who make donations 
Predictors:
        Highest degree of education
Major 
length of the stay in this university
received any kind of finicial aids from the university or not
annual income

library(datasets)
library(factoextra)
library(gridExtra)
library(purrr)
library(outliers)

#Question 4.2
Use the R function kmeans to cluster the points as well as possible. Report the best combination of
predictors, your suggested value of k, and how well your best clustering predicts flower type.
head(iris)
str(iris)
?kmeans

#only keep the predictor variables
iris4.2<-iris[,1:4]
head(iris4.2)

Scale to the normal distribution
meansl=mean(iris4.2$Sepal.Length)
sdsl=sd(iris4.2$Sepal.Length)

meansw=mean(iris4.2$Sepal.Width)
sdsw=sd(iris4.2$Sepal.Width)

meanpl=mean(iris4.2$Petal.Length)
sdpl=sd(iris4.2$Petal.Length)

meanpw=mean(iris4.2$Petal.Width)
sdpw=sd(iris4.2$Petal.Width)

attach(iris4.2)
iris4.2$s.lgth<-(Sepal.Length-meansl)/sdsl
iris4.2$s.wdth<-(Sepal.Width-meansw)/sdsw
iris4.2$p.lgth<-(Petal.Length-meanpl)/sdpl
iris4.2$p.wdth<-(Petal.Width-meanpw)/sdpw

only keep the scaled data
irisf<-iris4.2[,5:8]
head(irisf)

set.seed(1234)
#Method 1. Using all predictors (Septal Length, Septal Width,Petal Length, and Petal Width)
k2 <- kmeans(irisf, centers=  2, nstart = 25)
k3 <- kmeans(irisf, centers = 3, nstart = 25)
k4 <- kmeans(irisf, centers = 4, nstart = 25)
k5 <- kmeans(irisf, centers = 5, nstart = 25)

# function to compute total within-cluster sum of square 
wss <- function(k) {
        kmeans(irisf, k, nstart = 25 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
kvalues <- 1:15

# extract wss for 1-15 clusters
wss_values <- map_dbl(kvalues, wss)

plot(kvalues, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

3 is the desired number of clusters
table(k3$cluster, iris$Species)
all the setosa was perfectly clustered in one group, while the versicolor and virginica were clustered in 2 groups

#Method 2. Using Two predictors (Septal Length, Septal Width)
slw2 <- kmeans(irisf[,1:2], centers=  2, nstart = 25)
slw3 <- kmeans(irisf[,1:2], centers = 3, nstart = 25)
slw4 <- kmeans(irisf[,1:2], centers = 4, nstart = 25)
slw5 <- kmeans(irisf[,1:2], centers = 5, nstart = 25)

# function to compute total within-cluster sum of square 
wssslw <- function(k) {
        kmeans(irisf[1:2], k, nstart = 25 )$tot.withinss
}

# extract wss for 1-15 clusters
wss_valuesslw <- map_dbl(kvalues, wssslw) #kvalues are the same to the previous, which was 1:15

plot(kvalues, wss_valuesslw,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

Same to the previous one,3 is the desired number of clusters,where we got significant improvement.
table(slw3$cluster, iris$Species)
Compare to the previous method, I don't see great improvement in this one. Instead, setosa species was clustered into two groups. 

#Method 3. Using Two predictors (Pettal Length, Pettal Width)

plw2 <- kmeans(irisf[,3:4], centers=  2, nstart = 25)
plw3 <- kmeans(irisf[,3:4], centers = 3, nstart = 25)
plw4 <- kmeans(irisf[,3:4], centers = 4, nstart = 25)
plw5 <- kmeans(irisf[,3:4], centers = 5, nstart = 25)

# function to compute total within-cluster sum of square 
wssplw <- function(k) {
kmeans(irisf[3:4], k, nstart = 25 )$tot.withinss
}

# extract wss for 1-15 clusters
wss_valuesplw <- map_dbl(kvalues, wssplw) #kvalues are the same to the previous, which was 1:15

plot(kvalues, wss_valuesplw,
type="b", pch = 19, frame = FALSE, 
xlab="Number of clusters K",
ylab="Total within-clusters sum of squares")

Same to the previous ones,3 is the desired number of clusters,where we got significant improvement.
table(plw3$cluster, iris$Species)

# plots for visualization
p3 <- fviz_cluster(plw3, geom = "point",  data = irisf) + ggtitle("k = 3")
grid.arrange(p3)

Compare to the previous methods, this one is much better:Setosa was perfectly clustered as the method 1, and versicolor and virginica were also nicely clustered with only 2-4 misclassification.
I would recommend to use Pettal Length and Pettal Width as predictors, k value equals to 3 for this model. It can correctly predicts 144 out of 150 cluters.

#Using crime data from the file uscrime.txt (http://www.statsci.org/data/general/uscrime.txt,
description at http://www.statsci.org/data/general/uscrime.html), test to see whether there are any
outliers in the last column (number of crimes per 100,000 people). Use the grubbs.test function in
the outliers package in R.

crime<- read.table("http://www.statsci.org/data/general/uscrime.txt",header=TRUE)
head(crime)

crime<-crime$Crime

#Test if the lowest and highest value are two outliers on opposite tails of sample.
grubbs.test(crime,type=11)
p-value=1, so at least one of the values is not an outlier

#Test if the highest value is a outlier.
grubbs.test(crime,type=10)
if we set the threshold p-value=0.05, then we didn't detect the outlier.
if we set the threshold p-value=0.1, then we detected highest value 1993 as an outlier.

#Test if the lowest value is a outlier.
grubbs.test(crime,type=10,opposite=TRUE )
The p-value rounds to 1, so the lowest-crime city does not 
p-value=1, so the lowest value is not a outlier. This is consistent with our first test,where we found at least one of the extreme values is not an outlier. 
In conclusion,the highest value is an outlier at p-value=0.1; it is not an outlier at p-value=0.05.

#Question 6.1
Describe a situation or problem from your job, everyday life, current events, etc., for which a Change
Detection model would be appropriate. Applying the CUSUM technique, how would you choose the
critical value and the threshold?

Machine may gets heated after running for a long time. Technicians would want to prevent it gets overheated before it's too late.
CUSUM can be used to monitor the temperature of the machine,and detect a change when the temperature gets above a certain threhold, 
indicating overheat.
The cost of can't detect the overheat in time is much greater than a false alarm. So the criticial value and the threhold would be 
some small values, based on the historical data.

#Question 6.2
1. Using July through October daily-high-temperature data for Atlanta for 1996 through 2015, use
a CUSUM approach to identify when unofficial summer ends (i.e., when the weather starts cooling off) 
each year. You can get the data that you need from the file temps.txt or online,
for example at http://www.iweathernet.com/atlanta-weather-records or
https://www.wunderground.com/history/airport/KFTY/2015/7/1/CustomHistory.html . You can
use R if you'd like, but it's straightforward enough that an Excel spreadsheet can easily do the
job too.
2. Use a CUSUM approach to make a judgment of whether Atlanta's summer climate has gotten
warmer in that time (and if so, when).

temp<-read.table("F:/GT-OMSA/6501-Introduction to Analytics Modeling/hw2/temps.txt",stringsAsFactors = FALSE, header = TRUE)
head(temp)
tail(temp)
mean1996<-mean(temp$X1996)
mean1997<-mean(temp$X1997)
mean1998<-mean(temp$X1998)
mean1999<-mean(temp$X1999)
