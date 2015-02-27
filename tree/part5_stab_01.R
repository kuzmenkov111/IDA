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

cartDT = function(trainData, testData=NULL, formula, ntree = 2, ...) {
  if(class(trainData) != "data.frame" & ifelse(is.null(testData),TRUE,class(testData)=="data.frame"))
    stop("data frames should be entered for train and test (if any) data")  
  # extract response name and index
  res.name = gsub(" ","",unlist(strsplit(formula,split="~"))[[1]])
  res.ind = match(res.name, colnames(trainData))
  if(res.name %in% colnames(trainData) == FALSE)
    stop("response is not found in train data")
  if(!class(trainData[,res.ind]) %in% c("factor","numeric"))
      stop("response should be either factor or numeric")
  if(!("rpart" %in% rownames(installed.packages()) & "mlr" %in% rownames(installed.packages())))
    stop("rpart or mlr packages is not installed")  
  # source classes and utilities
  source("src/cart.R")
  source("src/mlUtils.R")
  # create base class
  rpt = tryCatch({
    cartRPART(trainData,testData,formula=formula)
  },
  error=function(cond) {
    message("cartRPART is not sourced")
    list()
  })
  
  require(rpart)
  require(mlr)
  # set up variables
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
  mge.oob = data.frame(ind=as.numeric(rownames(trainData))) # oob response
  mge.oob.lst = mge.oob # oob fitted at lst
  mge.oob.se = mge.oob # oob fitted by 1-SE
  if(!is.null(testData)) {
    mge.tst = list() # tst actual
    mge.tst.lst = mge.tst # tst fitted at lst
    mge.tst.se = mge.tst # tst fitted at se
  }
  
  cnt = 0
  while(cnt < ntree) {  
    # create resample description and task
    if(class(trainData[,res.ind]) != "factor") {
      boot.desc = makeResampleDesc(method="Bootstrap", stratify=FALSE, iters=1)
      boot.task = makeRegrTask(data=trainData,target=res.name)
    } else {
      boot.desc = makeResampleDesc(method="Bootstrap", stratify=TRUE, iters=1)
      boot.task = makeClassifTask(data=trainData,target=res.name)
    }
    # create bootstrap instance and split data - in-bag and out-of-bag
    boot.ins = makeResampleInstance(desc=boot.desc, task=boot.task) 
    trn.in = trainData[boot.ins$train.inds[[1]],]
    trn.oob = trainData[boot.ins$test.inds[[1]],]
    # fit model on bootstrap sample
    mod = rpart(formula=formula, data=trn.in, control=rpart.control(cp=0))
    cp = tryCatch({
      unlist(bestParam(mod$cptable,"CP","xerror","xstd")[1,1:2])
    },
    error=function(cond) { 
      message("cp fails to be generated. A sample is not taken.")
      cp = c(0,0)
    })  
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
      if(!is.null(testData)) {
        colName = paste("s",cnt,sep=".")
        fit.tst[[length(fit.tst)+1]] = data.frame(testData[, match(res.name, colnames(testData))],row.names=NULL)
        colnames(fit.tst[[length(fit.tst)]])=colName
        fit.tst.lst[[length(fit.tst.lst)+1]] = as.data.frame(fitData(prune.lst, testData, res.name, cnt)[,2])
        colnames(fit.tst.lst[[length(fit.tst.lst)]])=colName
        fit.tst.se[[length(fit.tst.se)+1]] = as.data.frame(fitData(prune.se, testData, res.name, cnt)[,2])
        colnames(fit.tst.se[[length(fit.tst.se)]])=colName
      }
    }
  }
  
  if(length(fit.tst) > 0) mge.tst = do.call(cbind, fit.tst)
  if(length(fit.tst.lst) > 0) mge.tst.lst = do.call(cbind, fit.tst.lst)
  if(length(fit.tst.se) > 0) mge.tst.se = do.call(cbind, fit.tst.se)
  
  # create result
  if(!is.null(testData)) {
    result = list(rpt=rpt, boot.cp=boot.cp, varImp.lst=varImp.lst, varImp.se=varImp.se, res.oob=res.oob
                  ,fit.oob.lst=fit.oob.lst, fit.oob.se=fit.oob.se, fit.tst=fit.tst, fit.tst.lst=fit.tst.lst
                  ,fit.tst.se=fit.tst.se, mge.oob, mge.oob.lst, mge.oob.se, mge.tst=mge.tst
                  ,mge.tst.lst=mge.tst.lst, mge.tst.se=mge.tst.se)
  } else {
    result = list(rpt=rpt, boot.cp=boot.cp, varImp.lst=varImp.lst, varImp.se=varImp.se, res.oob=res.oob
                  ,fit.oob.lst=fit.oob.lst, fit.oob.se=fit.oob.se, fit.tst=fit.tst, fit.tst.lst=fit.tst.lst
                  ,fit.tst.se=fit.tst.se, mge.oob, mge.oob.lst, mge.oob.se)
  }
  class(result) = c("rpartDT","rpartExt")
  return(result)
}

formula = "High ~ ."
tmp = cartDT(trainData.cl, testData.cl, formula, ntree=2)






