library(data.table)
library("smooth")
setwd("G:/a-XiaoWang/OMSA/ISYE-6501/hw2")
#Question 7.2
Using the 20 years of daily high temperature data for Atlanta (July through October) from Question 6.2
(file temps.txt), build and use an exponential smoothing model to help make a judgment of whether
the unofficial end of summer has gotten later over the 20 years. 
temps<-read.table("//cdc.gov/private/L137/yks5/OMSA/ISYE6501/hw3/temps.txt",header=TRUE)
#convert to time series
temps<-read.table("temps.txt",header=TRUE)
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

#We can see from the picture that the in-sample forecasts agree pretty well with the observed values

#going to look at seasonal factors
head(temps_hw$fitted)
tail(temps_hw$fitted)

temps_hw_sf<-matrix(temps_hw$fitted[,4],nrow=123)
head(temps_hw_sf)
temps_hw_smoothed<-matrix(temps_hw$fitted[,1],nrow=123)
temps_hw_smoothed

#I am using the predicted value (Xhat) and the observed value (data from original temps.txt) to see if summer has gotten later over the 20 years.
#When the difference between the predicted value and the observed value reached max, which means predicted value is much higher than the observed value,
#among the whole period, I assume that day is the summer end date (the model thought the temperature was still high, while the actual temperature already went down). 
#Then I compare the date from year 1997 to year 2015 to see how the date varies.
fit<-temps_hw$fitted
y1997f<-fit[1:123,1]
y1997t<-temps[,2]
y1997<-cbind(y1997f,y1997t)
y1997$dif1997<-y1997$y1997f-y1997$y1997t

y1997<- data.frame(fit = fit[1:123,1], obs = temps[,2])
y1997$dif <-y1997$fit - y1997$obs
which.max(y1997$dif)

y1998<- data.frame(fit = fit[124:246,1], obs = temps[,3])
y1998$dif <-y1998$fit - y1998$obs
which.max(y1998$dif)

















