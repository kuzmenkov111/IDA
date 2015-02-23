## data
require(ISLR)
data(Carseats)
require(dplyr)
Carseats = Carseats %>% 
  mutate(High=factor(ifelse(Sales<=8,"No","High"),labels=c("High","No")))
data.cl = subset(Carseats, select=c(-Sales))
data.rg = subset(Carseats, select=c(-High))

## import constructors
source("src/cart.R")

# split - cl: classification, rg: regression
require(caret)
set.seed(1237)
trainIndex = createDataPartition(Carseats$High, p=0.8, list=FALSE, times=1)
trainData.cl = data.cl[trainIndex,]
testData.cl = data.cl[-trainIndex,]
trainData.rg = data.rg[trainIndex,]
testData.rg = data.rg[-trainIndex,]

## classification
cartRPART = function(trainData, testData=NULL, formula, ...) {
  if(class(trainData) != "data.frame" & ifelse(is.null(testData),TRUE,class(testData)=="data.frame"))
    stop("data frames should be entered for train and test (if any) data")  
  # extract response name and index
  res.name = gsub(" ","",unlist(strsplit(formula,split="~"))[[1]])
  res.ind = match(res.name, colnames(trainData))
  if(res.name %in% colnames(trainData) == FALSE)
    stop("response is not found in train data")
  if(class(trainData[,res.ind]) != "factor") {
    if(class(trainData[,res.ind]) != "numeric") {
      stop("response should be either numeric or factor")
    }
  } 
  if("rpart" %in% rownames(installed.packages()) == FALSE)
    stop("rpart package is not installed")
  
  ## import utility functions
  source("src/mlUtils.R")
  
  require(rpart)
  # rpart model - $mod (1)
  mod = rpart(formula=formula, data=trainData, control=rpart.control(cp=0))
  # min/best cp - $cp (2)
  cp = bestParam(mod$cptable,"CP","xerror","xstd")
  # performance
  perf = function(model,data,response,params,isTest=TRUE,isSE=TRUE) {
    param = ifelse(isSE,params[1,2],params[1,1])
    fit = prune(model, cp=param)
    if(class(trainData[,res.ind]) == "factor") {
      ftd = predict(fit, newdata=data, type="class")
      cm = table(actual=data[,res.ind], fitted=ftd)
      cm.up = updateCM(cm,type=ifelse(isTest,"Ptd","Ftd"))
      err = cm.up[nrow(cm.up),ncol(cm.up)]
    } else {
      ftd = predict(fit, newdata=data)
      cm.up = regCM(data[,res.ind],ftd,probs=c(0.25,0.5,0.75),ifelse(isTest,"Ptd","Ftd"))
      err = round(sqrt(sum(data[,res.ind]-ftd)^2/length(data[,res.ind])),6)
    }
    error = list(pkg="rpart",isTest=as.logical(isTest),isSE=as.logical(isSE)
                 ,cp=as.numeric(round(param,4)),error=as.numeric(err))
    list(ftd=ftd,cm=cm.up,error=error)
  }
  # $train.lst (3)
  train.perf.lst = perf(mod,trainData,res.name,cp,FALSE,FALSE)
  # $train.se (4)
  train.perf.se = perf(mod,trainData,res.name,cp,FALSE,TRUE)
  if(!is.null(testData)) {
    # $test.lst (5)
    test.perf.lst = perf(mod,testData,res.name,cp,TRUE,FALSE)
    # $test.se (6)
    test.perf.se = perf(mod,testData,res.name,cp,TRUE,TRUE)
  } else {
    test.perf.lst = ls()
    test.perf.se = ls()
  }    
  # update results
  result = list(mod=mod,cp=cp
                ,train.lst=train.perf.lst,train.se=train.perf.se
                ,test.lst=test.perf.lst,test.se=test.perf.se)
  # add class name
  class(result) = "rpartExt"
  return(result)
}

cartDT = function(trainData, testData=NULL, formula, fitInd=FALSE, iters=10, ...) {
  if(class(trainData) != "data.frame" & ifelse(is.null(testData),TRUE,class(testData)=="data.frame"))
    stop("data frames should be entered for train and test (if any) data")
  # extract response name and index
  res.name = gsub(" ","",unlist(strsplit(formula,split="~"))[[1]])
  res.ind = match(res.name, colnames(trainData))
  if(res.name %in% colnames(trainData) == FALSE)
    stop("response is not found in train data")
  if(class(trainData[,res.ind]) != "factor") {
    if(class(trainData[,res.ind]) != "numeric") {
      stop("response should be either numeric or factor")
    }
  } 
  if("rpart" %in% rownames(installed.packages()) == FALSE)
    stop("rpart package is not installed")
  if("mlr" %in% rownames(installed.packages()) == FALSE)
    stop("mlr package is not installed")
  
  # fit by rpart package if necessary
  if(fitInd) {
    if(!is.null(ls()[ls()=="cartRPART"])) {
      rpt = cartRPART(trainData,testData,formula=formula)
    } else {
      message("cartRPART is not found")
      message("data is not fit by rpart")
      rpt = ls()
    }
  } else {
    message("data is not fit by rpart")
    rpt = ls()
  }
  # create bootstrap samples
  require(mlr)
  resample.desc = makeResampleDesc(method="Bootstrap",iters=iters)
  resample.Inds = makeResampleInstance(desc=resample.desc
                                       ,task=makeClassifTask(data=trainData,target=res.name))
}

res.name="High"

require(mlr)
set.seed(127)
resample.desc = makeResampleDesc(method="Bootstrap",iters=2)
resample.Inds = makeResampleInstance(desc=resample.desc
                                     ,task=makeClassifTask(data=trainData.cl,target="High"))


rpt.cl = cartRPART(trainData.cl,testData.cl,formula="High ~ .")

set.seed(12357)
for(i in 1:length(resample.Inds)) {
  inb = trainData.cl[resample.Inds[[i]],]
  oob = trainData.cl[-resample.Inds[[i]],]
  tst = testData.cl
  rpt.cl = cartRPART(trainData.cl,testData.cl,formula="High ~ .")  
}


tmpDF = trainData.cl
man = as.factor(c(rep("Medium",300),rep("Good",21)))
tmpDF$ShelveLoc = man

rpart(formula="High ~ .", data=tmpDF, control=rpart.control(cp=0))


# variable importance
# oob error
# test error

head(trainData.cl[,sapply(trainData.cl, class)=="factor"])
















set.seed(12357)
rpt.rg = cartRPART(trainData.rg,testData.rg,formula="Sales ~ .")