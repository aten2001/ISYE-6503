library(data.table)

#Question 7.2
Using the 20 years of daily high temperature data for Atlanta (July through October) from Question 6.2
(file temps.txt), build and use an exponential smoothing model to help make a judgment of whether
the unofficial end of summer has gotten later over the 20 years. 
temps<-read.table("H:/aaa-XW/OMSA/ISYE-6501/hw3/temps.txt",header=TRUE)
head(temps)

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
# remove the response variable (it's in the 15th column)
vars = vars[-15]

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
summary(model[[24966]])
summary(model[[18494]])
summary(model[[11564]])
summary(model[[5817]])

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






