#(a) a regression tree model
```{r}
fita<-tree(Crime~.,data=crime)
plot(fita)
text(fita)
summary(fita)
```

Since we only have 47 data points for the crime data. I used the whole dataset to fit the tree model, instead of half of them. 
From the summary, I found that only "Po1" "Pop" "LF"  "NW"  are used in the construction of the tree. There are 7 terminal nodes.

check out how the tree was split
```{r}
fita$frame
fita$where
```

#Manually calculate R square to see how it fits
```{r}
yhat<-predict(fita)
plot(yhat,crime$Crime)
sse<-sum((yhat - crime$Crime) ^ 2)
sst<-sum((crime$Crime - mean(crime$Crime)) ^ 2) #total sum of squares
1 - sse / sst
```

#prune tree
```{r}
plot(prune.tree(fita)$size,prune.tree(fita)$dev)
```

#prune tree to 4 leaves is desired
```{r}
fit4<-prune.tree(fita,best=4)
plot(fit4)
text(fit4)

summary(fit4)
```
#Now Only po1 and NW was included, the residual mean deviance is 61220.

#calculate R square
```{r}
yhat4<-predict(fit4)
plot(yhat4,crime$Crime)
sse4<-sum((yhat4 - crime$Crime) ^ 2)
sst4<-sum((crime$Crime - mean(crime$Crime)) ^ 2) #total sum of squares
1 - sse4 / sst4
```

#The R square dropped from 0.7244962 to 0.6174017, which was expected, because we have fewer predictors left in the model.

#Now do a cross validate on the pruned tree
```{r}
cv<-cv.tree(fit4)
cv$dev
cv$size
```
#The deviance becomes 7608563, even larger, indicating our model is not a good fit.
