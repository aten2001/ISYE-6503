library(data.table)
library("smooth")

setwd("//cdc.gov/private/L137/yks5/OMSA/ISYE6501/hw3")
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

y1997<- data.frame(fit = fit[1:123,1], obs = temps[,2])
y1997$dif <-y1997$fit - y1997$obs
head(y1997)
a<-which.max(y1997$dif)
y1997[which.max(y1997$dif),]
#the 100th day is the day where predicted tempererature is much higher than the actual temperature.

y1998<- data.frame(fit = fit[124:246,1], obs = temps[,3])
y1998$dif <-y1998$fit - y1998$obs
b<-which.max(y1998$dif)

y1999<- data.frame(fit = fit[247:369,1], obs = temps[,3])
y1999$dif <-y1999$fit - y1999$obs
c<-which.max(y1999$dif)

y2000<- data.frame(fit = fit[370:492,1], obs = temps[,3])
y2000$dif <-y2000$fit - y2000$obs
d<-which.max(y2000$dif)

y2001<- data.frame(fit = fit[493:615,1], obs = temps[,3])
y2001$dif <-y2001$fit - y2001$obs
e<-which.max(y2001$dif)

y2002<- data.frame(fit = fit[616:738,1], obs = temps[,3])
y2002$dif <-y2002$fit - y2002$obs
f<-which.max(y2002$dif)

y2003<- data.frame(fit = fit[739:861,1], obs = temps[,3])
y2003$dif <-y2003$fit - y2003$obs
g<-which.max(y2003$dif)

y2004<- data.frame(fit = fit[862:984,1], obs = temps[,3])
y2004$dif <-y2004$fit - y2004$obs
h<-which.max(y2004$dif)

y2005<- data.frame(fit = fit[985:1107,1], obs = temps[,3])
y2005$dif <-y2005$fit - y2005$obs
i<-which.max(y2005$dif)

y2006<- data.frame(fit = fit[1108:1230,1], obs = temps[,3])
y2006$dif <-y2006$fit - y2006$obs
j<-which.max(y2006$dif)

y2007<- data.frame(fit = fit[1231:1353,1], obs = temps[,3])
y2007$dif <-y2007$fit - y2007$obs
k<-which.max(y2007$dif)

y2008<- data.frame(fit = fit[1354:1476,1], obs = temps[,3])
y2008$dif <-y2008$fit - y2008$obs
l<-which.max(y2008$dif)

y2009<- data.frame(fit = fit[1477:1599,1], obs = temps[,3])
y2009$dif <-y2009$fit - y2009$obs
m<-which.max(y2009$dif)

y2010<- data.frame(fit = fit[1600:1722,1], obs = temps[,3])
y2010$dif <-y2010$fit - y2010$obs
n<-which.max(y2010$dif)

y2011<- data.frame(fit = fit[1723:1845,1], obs = temps[,3])
y2011$dif <-y2011$fit - y2011$obs
o<-which.max(y2011$dif)

y2012<- data.frame(fit = fit[1846:1968,1], obs = temps[,3])
y2012$dif <-y2012$fit - y2012$obs
p<-which.max(y2012$dif)

y2013<- data.frame(fit = fit[1969:2091,1], obs = temps[,3])
y2013$dif <-y2013$fit - y2013$obs
q<-which.max(y2013$dif)

y2014<- data.frame(fit = fit[2092:2214,1], obs = temps[,3])
y2014$dif <-y2014$fit - y2014$obs
r<-which.max(y2014$dif)

y2015<- data.frame(fit = fit[2215:2337,1], obs = temps[,3])
y2015$dif <-y2015$fit - y2015$obs
s<-which.max(y2015$dif)

end<-c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
plot(end)

#The plot shows the end days stays within a range of 100 -120 days from July 1st. So I would
#say the summer does not get later over the 20 years.

#Question 8.2
Using crime data from http://www.statsci.org/data/general/uscrime.txt 
(file uscrime.txt,description at http://www.statsci.org/data/general/uscrime.html ), 
use regression (a useful R function is lm or glm) to predict the observed crime rate in a city with the following data:
        M = 14.0
So = 0
Ed = 10.0
Po1 = 12.0
Po2 = 15.5
LF = 0.640
M.F = 94.0
Pop = 150
NW = 1.1
U1 = 0.120
U2 = 3.6
Wealth = 3200
Ineq = 20.1
Prob = 0.04
Time = 39.0

#Show your model (factors used and their coefficients), the software output, and the quality of fit. 

crime<- read.table("http://www.statsci.org/data/general/uscrime.txt",header=TRUE)
head(crime)

model1<-lm(Crime~.,crime)
summary(model1)

#I am using 0.05 as the threshold. Based on the p-value, I will keep M,Ed,Ineq,Prob,to fit a new model.
model2<-lm(Crime~M+Ed+Ineq+Prob,crime)
summary(model2)

#Now only the Ed and Prob still remains significant. The adjusted R-squred dropped from 0.7078 to  0.1927. I will fit a new model to see what happens.
model3<-lm(Crime~Ed+Prob,crime)
summary(model3)

#Now only Prob remains significant. The adjusted R-squred dropped from 0.1927. to 0.1756. So the first model with all the variables included truns out to be
#the best model so far. Next, I will try to use every combination to find the "best" model.

# create a NULL vector called model so we have something to add our layers to
model=NULL

# create a vector of the dataframe column names used to build the formula

vars = names(crime)
# remove the response variable (it's in the 16th column)
vars = vars[-16]

# the combn function will run every different combination of variables and then run the lm
for(i in 1:length(vars)){
        xx = combn(vars,i)
        if(is.null(dim(xx))){
                fla = paste("Crime ~", paste(xx, collapse="+"))
                model[[length(model)+1]]=lm(as.formula(fla),data=crime)
        } else {
                for(j in 1:dim(xx)[2]){
                        fla = paste("Crime ~", paste(xx[1:dim(xx)[1],j], collapse="+"))
                        model[[length(model)+1]]=lm(as.formula(fla),data=crime) 
                }
        }
}

# see how many models were build using the loop above
length(model)

# create a vector to extract AIC and BIC values from the model variable
AICs = NULL
BICs = NULL
for(i in 1:length(model)){
        AICs[i] = AIC(model[[i]])
        BICs[i] = BIC(model[[i]])
}

#see which models were chosen as best by AIC and BIC
which(AICs==min(AICs))
which(BICs==min(BICs))

#see which variables are in those models, and the corresponding adjusted R-squared.
summary(model[[18494]])
summary(model[[24966]])
summary(model[[5817]])
summary(model[[11564]])

#From the output, we can see the first two are the same model, and the last two are the same model. I will compare these two models with the model1, which has all the variables included

AIC(model1,model[[18494]],model[[5817]])
BIC(model1,model[[18494]],model[[5817]])
data.table(model1=0.7078,model18494=0.7444,model5817=0.7307)

#In conclusion, I would say model18494 is the best model I can found. The equation of the model is:
#crime= -6426.10+93.32*M+180.12*Ed+102.65*Po1+ 22.34*M.F-6086.63*U1+187.35*U2+61.33*Ineq-3796.03*Prob  
#The corresponding AIC is 639.3151, BIC is 657.8166, and the adjusted R-squared is 0.7444

#Use the seleted model to find the crime rate in the city with data provided:
crimerate=-6426.10+93.32*14.0+180.12*10.0+102.65*12.0+ 22.34*94.0-6086.63*0.120+187.35*3.6+61.33*20.1-3796.03*0.04 
crimerate

testpoint<-data.frame(M = 14.0,
                      So = 0,
                      Ed = 10.0,
                      Po1 = 12.0,
                      Po2 = 15.5,
                      LF = 0.640,
                      M.F = 94.0,
                      Pop = 150,
                      NW = 1.1,
                      U1 = 0.120,
                      U2 = 3.6,
                      Wealth = 3200,
                      Ineq = 20.1,
                      Prob = 0.04,
                      Time = 39.0)
preditc<-predict(model[[18494]],testpoint)

#perfrom 4-fold cross validation with the linear models that was choosen;
qqnorm(crime$Crime)
set.seed(1234)
lm1<-cv.lm(crime,model1,m=4)
lml18494<-cv.lm(crime,model[[18494]],m=4)

# We can calculate the R-squared values directly.
# R-squared = 1 - SSEresiduals/SSEtotal
#
# total sum of squared differences between data and its mean

SStot <- sum((crime$Crime - mean(crime$Crime))^2)
SStot

# for model, model2, and cross-validation, calculated SEres

SSres_model <- sum(model$residuals^2)

SSres_model2 <- sum(model2$residuals^2)

SSres_c <- attr(c,"ms")*nrow(dat) # mean squared error, times number of data points, gives sum of squared errors




