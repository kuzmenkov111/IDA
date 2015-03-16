#
# cartRPART() is a constructor that extends rpart object by rpart package
#
# Usage
# http://jaehyeon-kim.github.io/r/2015/02/21/Quick-Trial-of-Turning-Analysis-into-S3-Object/
# The above post explains the original object while minor modifications have been made since then.
#
# last modified on Mar 16, 2015
#
cartRPART = function(formula, trainData, testData=NULL, ...) {
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
  # fit model
  require(rpart)
  mod = rpart(formula=formula, data=trainData, control=rpart.control(cp=0))
  # select cp values by least xerror and 1-SE rule
  getCP = function(tbl,param,error,errStd, ...) {
    if(class(tbl)!="matrix")
      stop("cp table of rpart model required")
    # convert name to index
    ind = function(name, df) { match(name, colnames(df)) }
    param = ind(param, tbl)
    error = ind(error, tbl)
    errStd = ind(errStd, tbl)
    if(is.na(param) || is.na(error) || is.na(errStd))
      stop("CP, xerrr or xstd column doesn't exist")  
    # select cp by least xerror
    if(class(tbl[tbl[,error]==min(tbl[,error]),]) == "maxtrix") {
      message("multiple min xerror values, first one taken")
      lst = tbl[tbl[,error]==min(tbl[,error]),][1,]
    } else {
      lst = tbl[tbl[,error]==min(tbl[,error]),]
    }
    pick = c(lst[[param]],lst[[error]],lst[[errStd]])
    out = data.frame(lowest=pick, row.names=c("param","error","errStd"))
    # select cp by best xerror
    if(nrow(tbl) < 2) {
      bst = pick
    } else {
      bst = tbl[tbl[,error]<=pick[2]+pick[3],]
      bst = bst[1,c(param,error,errStd)]
    }
    out[,2] = data.frame(best=bst)
    out
  }  
  cp = getCP(mod$cptable, "CP", "xerror", "xstd")
  # output
  out = function(model, data, params, res, isLst=TRUE, isTrain=TRUE) {
    ind = match(res, colnames(data))
    param = if(isLst) params[1,1] else params[1,2]
    fit = prune(model, cp=param)
    # error
    if(class(data[,ind])=="factor") {
      ftd = predict(fit, newdata=data, type="class")
      cm = table(actual=data[,ind], fitted=ftd)
      err = 1 - sum(diag(cm))/sum(cm)
    } else {
      ftd = predict(fit, newdata=data)
      err = sqrt(sum(data[,ind]-ftd)^2/length(data[,ind]))
    }    
    if(isLst) {
      cp = params[1,1]
      xerror = params[2,1]
      xstd = params[3,1]
    } else {
      cp = params[1,2]
      xerror = params[2,2]
      xstd = params[3,2]
    }
    if(isTrain) {
      list(mod=fit, cp=cp, xerror=xerror, xstd=xstd, fitted=ftd
           ,var.importance=model$variable.importance, error=err)
    } else {
      list(fitted=ftd, error=err)
    }
  }
  result = list(train.lst=out(mod, trainData, cp, res.name, isLst=TRUE)
                ,test.lst=if(!is.null(testData)) out(mod, testData, cp, res.name, TRUE, FALSE) else NULL
                ,train.se=out(mod, trainData, cp, res.name, isLst=FALSE)
                ,test.se=if(!is.null(testData)) out(mod, testData, cp, res.name, FALSE, FALSE) else NULL)
  # assign classes
  class(result) = "rpartExt"
  return(result)
}

## data
require(ISLR)
data(Carseats)
require(dplyr)
Carseats = Carseats %>% 
  mutate(High=factor(ifelse(Sales<=8,"No","High"),labels=c("High","No")))
data.cl = subset(Carseats, select=c(-Sales))
data.rg = subset(Carseats, select=c(-High))

# split - cl: classification, rg: regression
require(caret)
set.seed(1237)
trainIndex = createDataPartition(Carseats$High, p=0.8, list=FALSE, times=1)
trainData.cl = data.cl[trainIndex,]
testData.cl = data.cl[-trainIndex,]
trainData.rg = data.rg[trainIndex,]
testData.rg = data.rg[-trainIndex,]

set.seed(12357)
cl = cartRPART(formula="High ~ .", trainData.cl, NULL)
rg = cartRPART(formula="Sales ~ .", trainData.rg, NULL)













