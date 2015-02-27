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

trn = trainData.cl
tst = testData.cl
formula = "High ~ ."
ntree = 2

source("src/mlUtils.R")
require(rpart)
require(mlr)
res.name = gsub(" ","",unlist(strsplit(formula,split="~"))[[1]])
res.ind = match(res.name, colnames(trn))

boot.cp = list()
varImp.lst = list()
varImp.se = list()
fit.oob.lst = list()
fit.oob.se = list()
fit.test.lst = list()
fit.test.se = list()
cnt = 0
while(cnt < ntree) {  
  # create resample description and task
  if(class(trn[,res.ind]) != "factor") {
    boot.desc = makeResampleDesc(method="Bootstrap", stratify=FALSE, iters=1)
    boot.task = makeRegrTask(data=trn,target=res.name)
  } else {
    boot.desc = makeResampleDesc(method="Bootstrap", stratify=TRUE, iters=1)
    boot.task = makeClassifTask(data=trn,target=res.name)
  }
  # create bootstrap instance
  boot.ins = makeResampleInstance(desc=boot.desc, task=boot.task) 
  # split train data in-bag and out-of-bag
  trn.in = trn[boot.ins$train.inds[[1]],]
  trn.out = trn[boot.ins$test.inds[[1]],]
  # fit model on bootstrap sample
  mod = rpart(formula=formula, data=trn.in, control=rpart.control(cp=0))
  cp = tryCatch({
    unlist(bestParam(mod$cptable,"CP","xerror","xstd")[1,1:2])
  },
    error=function(cond) { 
      message("cp fails to be generated. This sample is not taken.")
      cp = c(0,0)
    }
  )  
  if(sum(cp) != 0) {
    boot.cp[[length(boot.cp)+1]] = cp
    # update important variables with cps at least xerror and 1-SE rule
    prune.lst = prune(mod, cp=cp[1])
    varImp.lst[[length(varImp.lst)+1]] = prune.lst$variable.importance
    prune.se = prune(mod, cp=cp[2])
    varImp.se[[length(varImp.se)+1]] = prune.se$variable.importance
    # update OOB
    fitData = function(model, data, response, params) {
      ind = match(response, colnames(data))
      type = ifelse(class(data[,ind])=="factor","class","vector")
      fit = predict(model, newdata=data, type=type)      
    }
    # update oob predictions
    fit.oob.lst[[length(fit.oob.lst)+1]] = fitData(prune.lst, trn.out, res.name, cp[1]) 
    fit.oob.se[[length(fit.oob.se)+1]] = fitData(prune.se, trn.out, res.name, cp[2])
    # update test predictions if exists
    if(!is.null(tst)) {
      fit.test.lst[[length(fit.test.lst)+1]] = fitData(prune.lst, tst, res.name, cp[1])
      fit.test.se[[length(fit.test.se)+1]] = fitData(prune.se, tst, res.name, cp[2])
    }    
    cnt = cnt + 1
  }
}




bag = function(trn, tst = NULL, formula, ntree=1, ...) {
  require(rpart)
  require(mlr)
  res.name = gsub(" ","",unlist(strsplit(formula,split="~"))[[1]])
  res.ind = match(res.name, colnames(trn))
  # may need to skip a certain boostrap sample
  cnt = 0
  while(cnt < ntree) {
    if(class(trn[,res.ind]) != "factor") {
      boot.desc = makeResampleDesc(method="Bootstrap", stratify=FALSE, iters=1)
      boot.task = makeRegrTask(data=trn,target=res.name)
    } else {
      boot.desc = makeResampleDesc(method="Bootstrap", stratify=TRUE, iters=1)
      boot.task = makeClassifTask(data=trn,target=res.name)
    }
    boot.ins = makeResampleInstance(desc=boot.desc, task=boot.task)    
    cnt = cnt + 1
  }  
}

ntree = 2


# classification
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