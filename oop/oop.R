## intro
# construct base class
emp = function(name) {
  if(length(name) != 1 && !is.character(name))
    stop("single string value of name necessary")
  # assign name to result
  result = name
  class(result) = "employee"
  return(result)
}

# extend base class
man = function(name, members) {
  # create base class (employee)
  result = employee(name)
  
  if(length(sapply(members,class)[sapply(members,class)=="character"]) != length(members))
    stop("a vector employees necessary")
  # combine name and members as a list
  result = list(name=result,members=members)
  class(result)=c("manager","employee")
  return(result)
}

tom = emp("Tom")
alice = emp("Alice")
nick = man("Nick",c(tom,alice))
nick

foo = c(1)
class(foo) = "employee"
class(foo)
man("John",foo)

## packages
library(dplyr)
library(rpart)
library(caret)
library(mlr)

## data
require(ISLR)
data(Carseats)
Carseats = Carseats %>% 
  mutate(High=factor(ifelse(Sales<=8,"No","High"),labels=c("High","No")))
data = subset(Carseats, select=c(-Sales))

## split data
set.seed(1237)
trainIndex = createDataPartition(Carseats$High, p=0.8, list=FALSE, times=1)
trainData = data[trainIndex,]
testData = data[-trainIndex,]

## model classes
source("src/mlUtils.R")
mod.rpart = function(trainData, testData=NULL, formula, ...) {
  if(class(trainData) != "data.frame" & ifelse(is.null(testData),TRUE,class(testData)=="data.frame"))
    stop("data frames should be entered for test and train data")  
  # extract formula name
  res.name = gsub(" ","",unlist(strsplit(formula,split="~"))[[1]])
  if(res.name %in% colnames(trainData) == FALSE)
    stop("response is not found in train data")
  if("rpart" %in% rownames(installed.packages()) == FALSE)
    stop("rpart package is not installed")
  
  require(rpart)
  # rpart model - $mod
  mod = rpart(formula=formula, data=trainData, control=rpart.control(cp=0))
  # min/best cp - $cp
  cp = bestParam(mod$cptable,"CP","xerror","xstd")
  # performance
  perf = function(model,data,response,params,isTest=TRUE,isBest=TRUE) {
    # get index of response
    res.ind = match(response, colnames(data))
    param = ifelse(isBest,params[1,2],params[1,1])
    fit = prune(model, cp=param)
    ftd = predict(fit, type="class")
    cm = table(actual=trainData[,res.ind], fitted=ftd)
    cm.up = updateCM(cm,type=ifelse(isTest,"Ptd","Ftd"))
    mmce = list(pkg="rpart",isTest,isBest,cp=round(param,4),mmce=cm.up[nrow(cm.up),ncol(cm.up)])
    list(ftd=ftd,cm=cm.up,mmce=mmce)
  }
  train.perf.lst = perf(mod,trainData,res.name,cp,FALSE,FALSE)
  train.perf.bst = perf(mod,trainData,res.name,cp,FALSE,TRUE)
  test.perf.lst = perf(mod,testData,res.name,cp,TRUE,FALSE)
  test.perf.bst = perf(mod,testData,res.name,cp,TRUE,TRUE)
    
  # update results
  result = list(mod=mod,cp=cp,train.lst=train.perf.lst
                ,train.bst=train.perf.bst,test.lst=test.perf.lst
                ,test.bst=test.perf.bst)

  # add class name
  class(result) = "modRPART"
  return(result)
}

set.seed(12357)
mod = mod.rpart(trainData,testData,formula="High ~ .")

plot.modRPART = function(mod) {
  require(ggplot2)
  df = as.data.frame(mod$mod$cptable)
  ubound = ifelse(mod$cp[2,1]+mod$cp[3,1]>max(df$xerror),max(df$xerror),mod$cp[2,1]+mod$cp[3,1])
  lbound = ifelse(mod$cp[2,1]-mod$cp[3,1]<min(df$xerror),min(df$xerror),mod$cp[2,1]-mod$cp[3,1])
  ggplot(data=df[1:nrow(df),], aes(x=CP,y=xerror)) + 
    scale_x_continuous() + scale_y_continuous() + 
    geom_line() + geom_point() +   
    geom_abline(intercept=ubound,slope=0, color="purple") + 
    geom_abline(intercept=lbound,slope=0, color="purple") + 
    geom_point(aes(x=mod$cp[1,2],y=mod$cp[2,2]),color="red",size=3) + 
    geom_point(aes(x=mod$cp[1,1],y=mod$cp[2,1]),color="blue",size=3)
}

plot(mod)