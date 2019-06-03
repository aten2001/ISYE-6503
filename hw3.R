library(data.table)
library("smooth")
#Question 7.2
Using the 20 years of daily high temperature data for Atlanta (July through October) from Question 6.2
(file temps.txt), build and use an exponential smoothing model to help make a judgment of whether
the unofficial end of summer has gotten later over the 20 years. 
temps<-read.table("//cdc.gov/private/L137/yks5/OMSA/ISYE6501/hw3/temps.txt",header=TRUE)
#convert to time series
temps_vec<-as.vector(unlist(temps[,2:21]))
temps_vec
plot(temps_vec)
temps_ts<-ts(temps_vec,start=1996,frequency = 123)
temps_ts
plot(temps_ts)

plot(decompose(temps_ts))

temps_hw<-HoltWinters(temps_ts,alpha = NULL, beta= NULL, gamma = NULL,seasonal = "multiplicative")
temps_hw

#decomposed<-decompose(temps_hw)
#plot(decomposed)

summary(temps_hw)
plot(temps_hw)

#going to look at seasonal factors
head(temps_hw$fitted)
tail(temps_hw$fitted)

temps_hw_sf<-matrix(temps_hw$fitted[,4],nrow=123)
head(temps_hw_sf)
temps_hw_smoothed<-matrix(temps_hw$fitted[,1],nrow=123)
temps_hw_smoothed
