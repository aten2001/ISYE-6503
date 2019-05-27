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
#Question 4.1
Use the R function kmeans to cluster the points as well as possible. Report the best combination of
predictors, your suggested value of k, and how well your best clustering predicts flower type.
head(iris)
str(iris)
?kmeans

only keep the predictor variables
iris4.1<-iris[,1:4]
head(iris4.1)

Scale to the normal distribution
meansl=mean(iris4.1$Sepal.Length)
sdsl=sd(iris4.1$Sepal.Length)

meansw=mean(iris4.1$Sepal.Width)
sdsw=sd(iris4.1$Sepal.Width)

meanpl=mean(iris4.1$Petal.Length)
sdpl=sd(iris4.1$Petal.Length)

meanpw=mean(iris4.1$Petal.Width)
sdpw=sd(iris4.1$Petal.Width)

attach(iris4.1)
iris4.1$s.lgth<-(Sepal.Length-meansl)/sdsl
iris4.1$s.wdth<-(Sepal.Width-meansw)/sdsw
iris4.1$p.lgth<-(Petal.Length-meanpl)/sdpl
iris4.1$p.wdth<-(Petal.Width-meanpw)/sdpw

only keep the scaled data
irisf<-iris4.1[,5:8]
head(iris4.1)

set.seed(1234)
k2 <- kmeans(irisf, centers=  2, nstart = 25)
k3 <- kmeans(irisf, centers = 3, nstart = 25)
k4 <- kmeans(irisf, centers = 4, nstart = 25)
k5 <- kmeans(irisf, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point",  data = irisf) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = irisf) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = irisf) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = irisf) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

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
all the setosa was clustered in one group, while the versicolor and virginica were clustered in 2 groups

