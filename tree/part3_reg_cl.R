library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)

## data
require(ISLR)
data(Carseats)
# label order changed from previous articles (No=1)
Carseats = Carseats %>% 
  mutate(High=factor(ifelse(Sales<=8,"No","High"),labels=c("No","High")))
# structure of predictors
str(subset(Carseats,select=c(-High,-Sales)))
# classification response summary
res.cl.summary = with(Carseats,rbind(table(High),table(High)/length(High)))
res.reg.summary = summary(Carseats$Sales)

eqProb = with(Carseats,length(Sales[Sales<=8])/length(Sales))

# split data
set.seed(1237)
trainIndex = createDataPartition(Carseats$High, p=.8, list=FALSE, times=1)
# classification
trainData.cl = subset(Carseats, select=c(-Sales))[trainIndex,]
testData.cl = subset(Carseats, select=c(-Sales))[-trainIndex,]
# regression
trainData.reg = subset(Carseats, select=c(-High))[trainIndex,]
testData.reg = subset(Carseats, select=c(-High))[-trainIndex,]

# response summary
train.res.cl.summary = with(trainData.cl,rbind(table(High),table(High)/length(High)))
test.res.cl.summary = with(testData.cl,rbind(table(High),table(High)/length(High)))

train.res.reg.summary = summary(trainData.reg$Sales)
test.res.reg.summary = summary(testData.reg$Sales)

## fit model
# set up train control
trControl = trainControl(method="repeatedcv",number=10,repeats=5)

# train classification model
set.seed(12357)
mod.cl = train(High ~ .
                    ,data=trainData.cl
                    ,method="rpart"
                    ,tuneLength=20
                    ,trControl=trControl)
set.seed(12357)
mod.reg = train(Sales ~ .
                ,data=trainData.reg
                ,method="rpart"
                ,tuneLength=20
                ,trControl=trControl)
set.seed(12357)
mod.reg.custom = rpart(Sales ~ ., data=trainData.reg, control=rpart.control(cp=0))
mod.reg.custom$cptable

bestCP = function(data,cp,err,std,decreasing=TRUE,...) {
  # get index of each column
  ind = function(name, df) { grep(name, colnames(df)) }
  cpInd = ind(cp,df)
  errInd = ind(err, df)
  stdInd = ind(std, df)
  # reorder if necessary
  data = as.data.frame(data)
  data = data[order(data[,cpInd],decreasing=decreasing),]
  # create subset to apply 1-SE rule - cp, xerror, xstd if rpart
  df = cbind(df[2:nrow(data),cpInd],abs(diff(df[,errInd])),df[1:nrow(data)-1,stdInd])
  colnames(df) <- c(cp,err,std)
  # take if diff(xerror) > xstd
  subDF = subset(df,df[,2]>df[,3])
  # if found, get the last row, otherwise take the first column
  if(nrow(subDF)>0) {
    subDF = df[nrow(subDF),]
  } else {
    subDF = data.frame(data[1,cpInd],data[1,errInd],data[1,stdInd])
    colnames(subDF) <- c(cp,err,std)
  }  
  subDF[1]
}

# http://stackoverflow.com/questions/26828901/warning-message-missing-values-in-resampled-performance-measures-in-caret-tra
# http://stackoverflow.com/questions/10503784/caret-error-when-using-anything-but-loocv-with-rpart
# select results at best tuned cp
subset(mod.cl$results,subset=cp==mod.cl$bestTune$cp)
subset(mod.reg$results,subset=cp==mod.reg$bestTune$cp)

# refit the model to the entire training data
cp = mod.eq.cost$bestTune$cp
mod.eq.cost = rpart(High ~ ., data=trainData, control=rpart.control(cp=cp))

# generate confusion matrix on training data
source("src/mlUtils.R")
fit.eq.cost = predict(mod.eq.cost, type="class")
fit.cm.eq.cost = table(data.frame(actual=trainData$High,response=fit.eq.cost))
fit.cm.eq.cost = getUpdatedCM(fit.cm.eq.cost)
fit.cm.eq.cost

pred.eq.cost = predict(mod.eq.cost, newdata=testData, type="class")
pred.cm.eq.cost = table(data.frame(actual=testData$High,response=pred.eq.cost))
pred.cm.eq.cost = getUpdatedCM(pred.cm.eq.cost)
pred.cm.eq.cost

## fit model with unequal cost
# update prior probabilities
# assuming that incorrectly classifying 'High' has 3 times costly
costs = c(2,1)
train.res.summary
prior.w.weight = c(train.res.summary[2,1] * costs[1]
                   ,train.res.summary[2,2] * costs[2])
priorUp = c(prior.w.weight[1]/sum(prior.w.weight)
            ,prior.w.weight[2]/sum(prior.w.weight))
priorUp

# loss matrix
loss.mat = matrix(c(0,2,1,0),nrow=2,byrow=TRUE)
loss.mat

# refit the model with the updated priors
# fit with updated prior
mod.uq.cost = rpart(High ~ ., data=trainData, parms=list(prior=priorUp), control=rpart.control(cp=cp))
# fit with loss matrix
# mod.uq.cost = rpart(High ~ ., data=trainData, parms=list(loss=loss.mat), control=rpart.control(cp=cp))

# generate confusion matrix on training data
fit.uq.cost = predict(mod.uq.cost, type="class")
fit.cm.uq.cost = table(data.frame(actual=trainData$High,response=fit.uq.cost))
fit.cm.uq.cost = getUpdatedCM(fit.cm.uq.cost)
fit.cm.uq.cost

pred.uq.cost = predict(mod.uq.cost, newdata=testData, type="class")
pred.cm.uq.cost = table(data.frame(actual=testData$High,response=pred.uq.cost))
pred.cm.uq.cost = getUpdatedCM(pred.cm.uq.cost)
pred.cm.uq.cost















