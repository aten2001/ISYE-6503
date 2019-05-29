install.packages("kernlab")
install.packages("kknn")
library(kernlab)
library(kknn)
library(caret)
setwd("//cdc.gov/private/L137/yks5/OMSA/ISYE6501/Homework1/week_1_data-summer/data 2.2")
card<-read.table("credit_card_data.txt")
head(card)
str(card)

#1. Using the support vector machine function ksvm contained in the R package kernlab, find a good classifier for this data. Show the equation of your classifier, and how well it classifies the data points in the full data set. (Don't worry about test/validation data yet; we'll cover that topic soon.)

#convert the dataset to matrix
data<-as.matrix(card)
class(data)
class(data[,11])
summary(data)
?ksvm 
model1<-ksvm(data[,1:10],as.factor(data[,11]),type="C-svc",
            kernel="vanilladot",C=100,scaled=TRUE)
# calculate a1.am
a <- colSums(model1@xmatrix[[1]] * model1@coef[[1]])
a
# calculate a0
a0 <- -model1@b
a0
# see what the model predicts
pred1 <- predict(model1,data[,1:10])
pred1
# see what fraction of the model's predictions match the actual classification
sum(pred1 == data[,11]) / nrow(data)

#Model2, set C=1 and create a new model
model2<-ksvm(data[,1:10],as.factor(data[,11]),type="C-svc",
             kernel="vanilladot",C=1,scaled=TRUE)
a2 <- colSums(model2@xmatrix[[1]] * model2@coef[[1]])
a2

a02 <- -model2@b
a02

pred2 <- predict(model2,data[,1:10])

# see what fraction of the model's predictions match the actual classification
p2<-sum(pred2 == data[,11]) / nrow(data)
p2

# the result are the same to the model1

#Model3, set C=.001 and create a new model
model3<-ksvm(data[,1:10],as.factor(data[,11]),type="C-svc",
             kernel="vanilladot",C=.001,scaled=TRUE)
a3 <- colSums(model3@xmatrix[[1]] * model3@coef[[1]])
a3

a03 <- -model3@b
a03

pred3 <- predict(model3,data[,1:10])

# see what fraction of the model's predictions match the actual classification
p3<-sum(pred3 == data[,11]) / nrow(data)
p3

p3-p2
# the result are the slightly worse than the model1 and model2. I will set C to a larger
# vlaue to see if it would improve the results

#Model4, set C=100000 and create a new model
model4<-ksvm(data[,1:10],as.factor(data[,11]),type="C-svc",
             kernel="vanilladot",C=100000,scaled=TRUE)
a4 <- colSums(model4@xmatrix[[1]] * model4@coef[[1]])
a4

a04<- -model4@b
a04

pred4 <- predict(model4,data[,1:10])

p4<-sum(pred4 == data[,11]) / nrow(data)
p4

p4-p3
p4-p2
# This model performs similiar to the model 1 and model 2

#Model5, set C=1000000 and create a new model
model5<-ksvm(data[,1:10],as.factor(data[,11]),type="C-svc",
             kernel="vanilladot",C=1000000,scaled=TRUE)
a5<- colSums(model5@xmatrix[[1]] * model5@coef[[1]])
a5

a05<- -model5@b
a05

pred5 <- predict(model5,data[,1:10])

p5<-sum(pred5 == data[,11]) / nrow(data)
p5

p5-p2
p5-p3
#This model is ever worse than the model3. So increasing the C not nessarily improve the result.
#The "best" C may be somewhere between (100000,1000000) and (.001,100), or c=100 is already the best solution.

#2. You are welcome, but not required, to try other (nonlinear) kernels as well; we're not covering them in this course, but they can sometimes be useful and might provide better predictions than vanilladot.

model6<-ksvm(data[,1:10],as.factor(data[,11]),type="C-svc",
             kernel="polydot",C=100,scaled=TRUE)
a6<- colSums(model6@xmatrix[[1]] * model6@coef[[1]])
a6

a06<- -model6@b
a06

pred6 <- predict(model6,data[,1:10])

p6<-sum(pred6 == data[,11]) / nrow(data)
p6

p6-p2
p6-p3

#This method provid similiar predictions as vanilladot, when c=100

#3. Using the k-nearest-neighbors classification function kknn contained in the R kknn package, suggest a good value of k, and show how well it classifies that data points in the full data set. Don't forget to scale the data (scale=TRUE in kknn).
#see how the kknn works
?kknn

acc<-function(k){

fit<-rep(0,(nrow(card))) #start with 0s.
        
for (i in 1:nrow(card)){
        train<-card[-i,] #exclude itself
        test<- card[i,]
modelkknn<-kknn(V11~.,train,test,k = k, kernel = "optimal", ykernel = NULL, scale=TRUE)

fit[i]<-as.integer(fitted(modelkknn)+0.5) #for rounding
}
# see what fraction of the model's predictions match the actual classification
        p223<-sum(fit == card[,11]) / nrow(card)    
        p223
}

# Now call the function for values of k from 3 to 15

test_vec <- rep(0,20) 
for (k in 3:20){
        test_vec[k] = acc(k) 
}

accuracy <- as.data.frame(test_vec * 100) #set accuracy as percentage
accuracy
max<-max(accuracy)
final<-subset(accuracy,test_vec * 100==max)
#when k=12 or k=15, we got max accuracy 85.32%

#Question 3.1
#Using the same data set (credit_card_data.txt or credit_card_data-headers.txt) as in Question 2.2, use the ksvm or kknn function to find a good classifier:

#(a) using cross-validation (do this for the k-nearest-neighbors model; SVM is optional); and
set.seed(1234)

#set a max number of k
kmax<-20
#start with 0s.
rate<-rep(0,kmax) #set initial value to 0

for (k in 1:kmax){
        model3a<-cv.kknn(V11~.,card,kcv=10,#10-fold cross-validation
                                     k = k,#number of neighbor, max=kmax=20
                         kernel = "optimal", ykernel = NULL, scale=TRUE)
        
        fit<-as.integer(model3a[[1]][,2]+0.5)  #round to 0 or 1
        rate[k]<-sum(fit==card$V11)/nrow(card)
}
acc_rate<-as.data.frame(rate * 100) #set accuracy as percentage
max3a<-max(acc_rate)
final3a<-subset(acc_rate,rate * 100==max3a)
final3a
#when k=13, we got max accuracy 85.62691%

#(b) splitting the data into training, validation, and test data sets (pick either KNN or SVM; the other is optional).
set.seed(1234)
#about 60% were selected for the train data
selecttrain<-sample(1:nrow(card),392) 
remain<-card[-selecttrain,]
b_train<-card[selecttrain,]
#the reamining 30% were equally divided into test and valid datasets
b_test<-remain[1:131,] 
b_valid<-remain[132:262,]

cvalue<-c(0.00001,0.0001,0.001,0.01,0.1,1,100,10000,100000,1000000)
p3b<-rep(0,10)

for (i in  1:10){
model3b<-ksvm(as.matrix(b_train[,1:10]),
              as.factor(b_train[,11]),
              C=cvalue[i],
              type="C-svc", kernel="vanilladot",scaled=TRUE)

pred3b <- predict(model3b,b_valid[,1:10])
p3b[i]<-sum(pred3b == b_valid$V11) / nrow(b_valid)

}
p3b[1:10]
acc_rate3b<-as.data.frame(p3b * 100) #set accuracy as percentage
max3b<-max(acc_rate3b)
final3b<-subset(acc_rate3b,p3b * 100==max3b)
final3b
#when c in c(0.01,0.1,1,100,10000,100000), we got the highest accuracy rate:91.60305%

#ues c=0.01 to re-train the model on the test dataset
model3r<-ksvm(as.matrix(b_train[,1:10]),
              as.factor(b_train[,11]),
              C=0.01,
              type="C-svc", kernel="vanilladot",scaled=TRUE)

pred3r <- predict(model3r,b_test[,1:10])
p3r<-sum(pred3r == b_test$V11) / nrow(b_test)
p3r*100
# Performance on test data = 82.44275%