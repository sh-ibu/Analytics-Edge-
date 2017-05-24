train = read.csv('pisa2009train.csv')
test = read.csv('pisa2009test.csv')
dim(train)
str(train)
tapply(train$readingScore,train$male,mean)
summary(train)
train=na.omit(train)
test =na.omit(test)
dim(train)
dim(test)
train$raceeth = relevel(train$raceeth,'White')
train$raceeth=relevel(train$raceeth,'White')
str(train)
lm.fit1=lm(readingScore~.,data=train)
summary(lm.fit1)
RMSE = sqrt(mean(((lm.fit1$residuals)^2)))
RMSE
myprediction = predict(lm.fit1,newdata=test)
max(myprediction)-min(myprediction)
SSE = sum((myprediction-test$readingScore)^2)
SSE
RMSE=sqrt(SSE/nrow(test))
RMSE
#Base Line Model Prediction
mean(train$readingScore)
sum((test$readingScore-mean(train$readingScore))^2)

