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
# may need to skip a certain boostrap sample, don't create whole samples
cnt = 0
if(ntree < 2)
  stop("ntree should be at least 1")

while(cnt < ntree) {
  # some tests(eg, values of a factor are not the same)
  useSample = FALSE
  if(TRUE) {
    useSample=TRUE
    boot.cp = list()
    varImp.lst = list()
    varImp.se = list()
  }  
  # only if tests passed
  if(useSample) {
    cnt = cnt + 1
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
    cp = unlist(bestParam(boot.mod$cptable,"CP","xerror","xstd")[1,1:2])
    boot.cp[[length(boot.cp)+1]] = cp
    # prune with cp values at least xerror and 1-SE rule
    prune.lst = prune(boot.mod, cp=cp[1])
    varImp.lst[[length(varImp.lst)+1]] = prune.lst$variable.importance
    prune.se = prune(boot.mod, cp=cp[2])
    varImp.se[[length(varImp.se)+1]] = prune.se$variable.importance
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