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

cartDT = function(trainData, testData=NULL, formula, ntree = 2, mtry = NULL, ...) {
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
    stop("rpart or mlr package is not installed")  
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
  ## set up variables
  # cp values to keet at each sample at lowest xerror and by 1-SE rule
  mge.boot.cp = data.frame(measure=c("lowest","best"))
  # variable importance at each sample
  # standardized individual and cummulative variable importance will also be calculated below
  mge.varImp.lst = data.frame(var=colnames(trainData[,-res.ind]))
  mge.varImp.se = mge.varImp.lst
  # oob response and fitted values at each sample
  mge.oob.lst = data.frame(ind=as.numeric(rownames(trainData)),res=trainData[,res.ind])
  mge.oob.se = mge.oob.lst
  # test response and fitted values at each sample
  if(!is.null(testData)) {
    mge.tst.lst = data.frame(ind=as.numeric(rownames(testData)),res=testData[,res.ind])
    mge.tst.se = mge.tst.lst
  }
  ## split data and fit model on in-bag sample
  # some samples that cause an error to obtain cp values will be discarded - create sample recursively
  cnt = 0
  while(cnt < ntree) {
    # res.ind changed below and should be updated at each trial
    res.ind = match(res.name, colnames(trainData))
    # create resample description and task
    if(class(trainData[,res.ind]) != "factor") {
      boot.desc = makeResampleDesc(method="Bootstrap", stratify=FALSE, iters=1)
      boot.task = makeRegrTask(data=trainData,target=res.name)
    } else {
      boot.desc = makeResampleDesc(method="Bootstrap", stratify=TRUE, iters=1)
      boot.task = makeClassifTask(data=trainData,target=res.name)
    }
    # create bootstrap instance and split data - in-bag and out-of-bag
    if(is.null(mtry)) {
      mtry=ncol(trainData)-1
    } else if (mtry > ncol(trainData)-1) {
      message("mtry greater than number of predictors, set to number of predictors")
      mtry=ncol(trainData)-1
    } else {
      mtry = round(mtry,0)
    }    
    boot.ins = makeResampleInstance(desc=boot.desc, task=boot.task) 
    indices = sample((1:ncol(trainData))[-res.ind],mtry)
    trn.in = trainData[boot.ins$train.inds[[1]],c(indices,res.ind)]
    trn.oob = trainData[boot.ins$test.inds[[1]],c(indices,res.ind)]
    res.ind = match(res.name, colnames(trn.in))
    # fit model on in-bag sample
    mod = rpart(formula=formula, data=trn.in, control=rpart.control(cp=0))
    cp = tryCatch({
      unlist(bestParam(mod$cptable,"CP","xerror","xstd")[1,1:2])
    },
    error=function(cond) { 
      message("cp fails to be generated. A sample is not taken.")
      cp = c(0,0)
    })
    # take sample only if cp values are obtained
    if(sum(cp) != 0) {
      cnt = cnt + 1
      colName = paste("s",cnt,sep=".")
      ## update cp
      boot.cp = data.frame(names(cp),cp)
      colnames(boot.cp) = c("measure",colName)
      mge.boot.cp = merge(mge.boot.cp,boot.cp,by="measure",all=TRUE)
      ## update variable importance with cps at least xerror and 1-SE rule
      # lst
      prune.lst = prune(mod, cp=cp[1])
      varImp.lst = data.frame(names(prune.lst$variable.importance),prune.lst$variable.importance)
      colnames(varImp.lst) = c("var",colName)
      mge.varImp.lst = merge(mge.varImp.lst,varImp.lst,by="var",all=TRUE)
      # se
      prune.se = prune(mod, cp=cp[2])
      varImp.se = data.frame(names(prune.se$variable.importance),prune.se$variable.importance)
      colnames(varImp.se) = c("var",colName)
      mge.varImp.se = merge(mge.varImp.se, varImp.se, by="var",all=TRUE)
      ## function to generate fitted values df
      fitData = function(model, data, response, count, col.name) {
        ind = match(response, colnames(data))
        type = ifelse(class(data[,ind])=="factor","class","vector")
        fit = predict(model, newdata=data, type=type)
        fitDF = data.frame(as.numeric(names(fit)),fit,row.names=NULL)
        colnames(fitDF) = c("ind",col.name)
        fitDF
      }
      ## update oob fitted and response values    
      # lst
      fit.oob.lst = fitData(prune.lst, trn.oob, res.name, cnt, colName)
      mge.oob.lst = merge(mge.oob.lst,fit.oob.lst,by="ind",all=TRUE)
      # se
      fit.oob.se = fitData(prune.se, trn.oob, res.name, cnt, colName)
      mge.oob.se = merge(mge.oob.se,fit.oob.se,by="ind",all=TRUE)
      ## update test fitted and response values
      if(!is.null(testData)) {
        # lst
        fit.tst.lst = fitData(prune.lst, testData, res.name, cnt, colName)
        mge.tst.lst = merge(mge.tst.lst, fit.tst.lst, by="ind", all=TRUE)
        # se
        fit.tst.se = fitData(prune.se, testData, res.name, cnt, colName)
        mge.tst.se = merge(mge.tst.se, fit.tst.se, by="ind", all=TRUE)
      }
    }
  } # end of while
  ## update individual and cumulative variable importance
  # lst
  if(ncol(mge.varImp.lst) > 0) {
    # remove variable colume at col 1
    rownames(mge.varImp.lst) = mge.varImp.lst[,1]
    mge.varImp.lst = mge.varImp.lst[,2:ncol(mge.varImp.lst)]
    # create ind and cum varImp data frames
    ind.varImp.lst = as.data.frame(apply(mge.varImp.lst,2,function(x) {rep(0,times=nrow(mge.varImp.lst))}))
    rownames(ind.varImp.lst) = rownames(mge.varImp.lst)
    cum.varImp.lst = ind.varImp.lst
    for(i in 1:ncol(mge.varImp.lst)) {
      ind.varImp.lst[,i] = mge.varImp.lst[,i]/sum(mge.varImp.lst[,i],na.rm=TRUE)
      if(i==1) cum.varImp.lst[,i] = ind.varImp.lst[,i]
      else cum.varImp.lst[,i] = rowSums(mge.varImp.lst[,1:i],na.rm=TRUE)/sum(mge.varImp.lst[,1:i],na.rm=TRUE)
    }
  }
  # se
  if(ncol(mge.varImp.se) > 0) {
    # remove variable colume at col 1
    rownames(mge.varImp.se) = mge.varImp.se[,1]
    mge.varImp.se = mge.varImp.se[,2:ncol(mge.varImp.se)]
    # create ind and cum varImp data frames
    ind.varImp.se = as.data.frame(apply(mge.varImp.se,2,function(x) {rep(0,times=nrow(mge.varImp.se))}))
    rownames(ind.varImp.se) = rownames(mge.varImp.se)
    cum.varImp.se = ind.varImp.se
    for(i in 1:ncol(mge.varImp.se)) {
      ind.varImp.se[,i] = mge.varImp.se[,i]/sum(mge.varImp.se[,i],na.rm=TRUE)
      if(i==1) cum.varImp.se[,i] = ind.varImp.se[,i]
      else cum.varImp.se[,i] = rowSums(mge.varImp.se[,1:i],na.rm=TRUE)/sum(mge.varImp.se[,1:i],na.rm=TRUE)
    }
  }
  ## update fitted values (individual, cumulative) and errors (mmce, rmse)
  # function to update fitted values - majority vote or average
  retCum = function(fit) {
    if(ncol(fit) < 2) {
      message("no fitted values")
      cum.fit = fit
    } else {
        cum.fit = as.data.frame(apply(fit,2,function(x) { rep(0,times=(nrow(fit))) }))
        cum.fit[,1:2] = fit[,1:2]
        rownames(cum.fit) = rownames(fit)
        for(i in 3:ncol(fit)) {
          if(class(fit[,1])=="factor") {
            # very slow, conventional apply doesn't work, to be improved
            retVote = function(x) {
              tbl = table(unlist(x)[!is.na(x)])
              ifelse(length(tbl)==0,NA,ifelse(min(tbl)==max(tbl),NA,names(which.max(tbl))))
            }
            #require(compiler)
            #retVoteCmp = cmpfun(retVote)
            #for(j in 1:ncol(fit)) cum.fit[j,i] = retVoteCmp(fit[j,2:i])
            #cum.fit[,i] = apply(fit[,2:i],1,retVote)
            for(j in 1:ncol(fit)) cum.fit[j,i] = retVote(fit[j,2:i])
          } else {
            cum.fit[,i] = apply(fit[,2:i],1,mean,na.rm=TRUE)
          }
        }  
      }
    cum.fit
  }  
  # function to updated errors - mmce or rmse
  retErr = function(fit) {
    err = data.frame(t(rep(0,times=ncol(fit))))
    colnames(err)=colnames(fit)
    for(i in 2:ncol(fit)) {
      cmpt = complete.cases(fit[,1],fit[,i])
      if(class(fit[,1])=="factor") {
        tbl=table(fit[cmpt,1],fit[cmpt,i])
        err[i] = 1 - sum(diag(tbl))/sum(tbl)
      } else {
        err[i] = sqrt(sum(fit[cmpt,1]-fit[cmpt,i])^2/length(fit[cmpt,1]))
      }    
    }
    err[2:length(err)]
  }
  # function to combine jobs
  combine = function(ind.fit) {
    if(ncol(ind.fit) > 0) {
      # set row names from values of index column and remove the index column 
      rownames(ind.fit) = ind.fit[,1]
      ind.fit = ind.fit[,-1]
      # update fitted values - majority vote or average
      cum.fit = retCum(ind.fit)
      # update error - mmce or rmse
      ind.fit.err = retErr(ind.fit)
      cum.fit.err = retErr(cum.fit)
    } else {
      cum.fit = NA
      ind.fit.err = NA
      cum.fit.err = NA
    }
    list(ind.fit,cum.fit,ind.fit.err,cum.fit.err)
  }
  ## update individual elements
  # oob lst
  com.oob.lst = combine(mge.oob.lst)
  mge.oob.lst = com.oob.lst[[1]]
  cum.oob.lst = com.oob.lst[[2]]
  mge.oob.lst.err = com.oob.lst[[3]]
  cum.oob.lst.err = com.oob.lst[[4]]  
  # oob se
  com.oob.se = combine(mge.oob.se)
  mge.oob.se = com.oob.se[[1]]
  cum.oob.se = com.oob.se[[2]]
  mge.oob.se.err = com.oob.se[[3]]
  cum.oob.se.err = com.oob.se[[4]]
  
  if(!is.null(testData)) {
    # test lst
    com.tst.lst = combine(mge.tst.lst)
    mge.tst.lst = com.tst.lst[[1]]
    cum.tst.lst = com.tst.lst[[2]]
    mge.tst.lst.err = com.tst.lst[[3]]
    cum.tst.lst.err = com.tst.lst[[4]]
    # test se
    com.tst.se = combine(mge.tst.se)
    mge.tst.se = com.tst.se[[1]]
    cum.tst.se = com.tst.se[[2]]
    mge.tst.se.err = com.tst.se[[3]]
    cum.tst.se.err = com.tst.se[[4]]
  }
  
  ## create result
  if(!is.null(testData)) {
    result = list(rpt=rpt, boot.cp=mge.boot.cp
                  ,varImp.lst=mge.varImp.lst, ind.varImp.lst=ind.varImp.lst, cum.varImp.lst=cum.varImp.lst
                  ,varImp.se=mge.varImp.se, ind.varImp.se=ind.varImp.se, cum.varImp.se=cum.varImp.se
                  ,ind.oob.lst=mge.oob.lst, ind.oob.lst.err=mge.oob.lst.err
                  ,cum.oob.lst=cum.oob.lst, cum.oob.lst.err=cum.oob.lst.err
                  ,ind.oob.se=mge.oob.se, ind.oob.se.err=mge.oob.se.err
                  ,cum.oob.se=cum.oob.se, cum.oob.se.err=cum.oob.se.err
                  ,ind.tst.lst=mge.tst.lst, ind.tst.lst.err=mge.tst.lst.err
                  ,cum.tst.lst=cum.tst.lst, cum.tst.lst.err=cum.tst.lst.err
                  ,ind.tst.se=mge.tst.se, ind.tst.se.err=mge.tst.se.err
                  ,cum.tst.se=cum.tst.se, cum.tst.se.err=cum.tst.se.err)
  } else {
    result = list(rpt=rpt, boot.cp=mge.boot.cp
                  ,varImp.lst=mge.varImp.lst, ind.varImp.lst=ind.varImp.lst, cum.varImp.lst=cum.varImp.lst
                  ,varImp.se=mge.varImp.se, ind.varImp.se=ind.varImp.se, cum.varImp.se=cum.varImp.se
                  ,ind.oob.lst=mge.oob.lst, ind.oob.lst.err=mge.oob.lst.err
                  ,cum.oob.lst=cum.oob.lst, cum.oob.lst.err=cum.oob.lst.err
                  ,ind.oob.se=mge.oob.se, ind.oob.se.err=mge.oob.se.err
                  ,cum.oob.se=cum.oob.se, cum.oob.se.err=cum.oob.se.err)
  }
  class(result) = c("rpartDT","rpartExt")
  return(result)
}

set.seed(12357)
cl = cartDT(trainData.cl, testData.cl, "High ~ .", ntree=10)
set.seed(12357)
rg = cartDT(trainData.rg, testData.rg, "Sales ~ .", ntree=10)
