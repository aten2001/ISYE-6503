#Question 7.2
Using the 20 years of daily high temperature data for Atlanta (July through October) from Question 6.2
(file temps.txt), build and use an exponential smoothing model to help make a judgment of whether
the unofficial end of summer has gotten later over the 20 years. 


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
AIC1<-AIC(model1)
AIC1

BIC1<-BIC(model1)
BIC1

#I am using 0.05 as the threshold. Based on the p-value, I will keep M,Ed,Ineq,Prob,to fit the new model.
model2<-lm(Crime~M+Ed+Ineq+Prob,crime)
summary(model2)
AIC2<-AIC(model2)
AIC2

BIC2<-BIC(model2)
BIC2
#Now only the Ed and Prob still remains significant. The adjusted R-squred dropped from 0.7078 to  0.1927. I will fit a new model to see what happens.
model3<-lm(Crime~Ed+Prob,crime)
summary(model3)

#Now only Prob remains significant. The adjusted R-squred dropped from 0.1927. to 0.1756.
AIC <- data.table(AIC1 = AIC1,AIC2 = AIC2,AIC3 = AIC3)
AIC
BIC <- data.table(BIC1 = BIC1,BIC2 = BIC2,BIC3 = BIC3)
BIC

AdjR<-data.table(AdjR1 = 0.7078,AdjR2 = 0.1927,AdjR3 = 0.1756)
AdjR



















