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

boot.cp = list() # cp values at least xerror (lst) and by 1-SE rule (se)
varImp.lst = list() # variable importance at lst
varImp.se = list() # variable importance by se
res.oob = list() # oob response values
fit.oob.lst = list() # fitted oob response values at lst
fit.oob.se = list() # fited oob response values by se
fit.tst = list() # test response values
fit.tst.lst = list() # fitted test response values at lst
fit.tst.se = list() # fitted test response values by se
# index of data to merge each sample's outcome
mge.oob = data.frame(ind=as.numeric(rownames(trn))) # oob response
mge.oob.lst = mge.oob # oob fitted at lst
mge.oob.se = mge.oob # oob fitted by 1-SE
if(!is.null(tst)) {
  mge.tst = list() # tst actual
  mge.tst.lst = mge.tst # tst fitted at lst
  mge.tst.se = mge.tst # tst fitted at se
}

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
  # create bootstrap instance and split data - in-bag and out-of-bag
  boot.ins = makeResampleInstance(desc=boot.desc, task=boot.task) 
  trn.in = trn[boot.ins$train.inds[[1]],]
  trn.oob = trn[boot.ins$test.inds[[1]],]
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
    cnt = cnt + 1
    # update cp
    boot.cp[[length(boot.cp)+1]] = cp
    # update important variables with cps at least xerror and 1-SE rule
    prune.lst = prune(mod, cp=cp[1])
    varImp.lst[[length(varImp.lst)+1]] = prune.lst$variable.importance
    prune.se = prune(mod, cp=cp[2])
    varImp.se[[length(varImp.se)+1]] = prune.se$variable.importance
    # update OOB
    fitData = function(model, data, response, count) {
      ind = match(response, colnames(data))
      type = ifelse(class(data[,ind])=="factor","class","vector")
      fit = predict(model, newdata=data, type=type)
      fitDF = data.frame(as.numeric(names(fit)),fit,row.names=NULL)
      colnames(fitDF) = c("ind",paste("s",count,sep="."))
      fitDF
    }
    # update oob fitted and response values    
    fit.oob.lst[[length(fit.oob.lst)+1]] = fitData(prune.lst, trn.oob, res.name, cnt)
    fit.oob.se[[length(fit.oob.se)+1]] = fitData(prune.se, trn.oob, res.name, cnt)
    res.oob[[length(res.oob)+1]] = data.frame(as.numeric(rownames(trn.oob))
                                              ,trn.oob[, match(res.name, colnames(trn.oob))],row.names=NULL)
    colnames(res.oob[[length(res.oob)]])=c("ind",paste("s",cnt,sep="."))
    # merge oob fitted and response values
    mge.oob.lst = merge(mge.oob.lst,fit.oob.lst[[length(fit.oob.lst)]],by="ind",all=TRUE)
    mge.oob.se = merge(mge.oob.se,fit.oob.se[[length(fit.oob.se)]],by="ind",all=TRUE)
    mge.oob = merge(mge.oob,res.oob[[length(res.oob)]],by="ind",all=TRUE)
    # update test fitted and response values
    if(!is.null(tst)) {
      colName = paste("s",cnt,sep=".")
      fit.tst[[length(fit.tst)+1]] = data.frame(tst[, match(res.name, colnames(trn.oob))],row.names=NULL)
      colnames(fit.tst[[length(fit.tst)]])=colName
      fit.tst.lst[[length(fit.tst.lst)+1]] = as.data.frame(fitData(prune.lst, tst, res.name, cnt)[,2])
      colnames(fit.tst.lst[[length(fit.tst.lst)]])=colName
      fit.tst.se[[length(fit.tst.se)+1]] = as.data.frame(fitData(prune.se, tst, res.name, cnt)[,2])
      colnames(fit.tst.se[[length(fit.tst.se)]])=colName
    }
  }
}

if(length(fit.tst) > 0) mge.tst = do.call(cbind, fit.tst)
if(length(fit.tst.lst) > 0) mge.tst.lst = do.call(cbind, fit.tst.lst)
if(length(fit.tst.se) > 0) mge.tst.se = do.call(cbind, fit.tst.se)





do.call(cbind,fit.test.lst)
merge(tt,z,by="row.names",all.x=TRUE)[,-(5:8)]
cbind(t, z[, "symbol"][match(rownames(t), rownames(z))])

rowInd = rownames(trn)
names(fit.oob.lst[[2]])

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