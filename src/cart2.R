#
# cartRPART() is a constructor that extends rpart object of rpart package
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
  if(res.name %in% colnames(trainData) == FALSE) stop("response is not found in train data")
  if(class(trainData[,res.ind]) != "factor") 
    if(class(trainData[,res.ind]) != "numeric") stop("response should be either numeric or factor")
  if("rpart" %in% rownames(installed.packages()) == FALSE) stop("rpart package is not installed")

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

#
# cartBGG() is a constructor that create a bagged tree
#
# Usage
#
# last modified on Mar 19, 2015
#
cartBGG = function(formula, trainData, testData=NULL, ntree=1, ...) {
  if(class(trainData) != "data.frame" & ifelse(is.null(testData),TRUE,class(testData)=="data.frame"))
    stop("data frames should be entered for train and test (if any) data")  
  # extract response name and index
  res.name = gsub(" ","",unlist(strsplit(formula,split="~"))[[1]])
  res.ind = match(res.name, colnames(trainData))
  if(res.name %in% colnames(trainData) == FALSE) stop("response is not found in train data")
  if(class(trainData[,res.ind]) != "factor") 
    if(class(trainData[,res.ind]) != "numeric") stop("response should be either numeric or factor")
  if("rpart" %in% rownames(installed.packages()) == FALSE) stop("rpart package is not installed")
  
  # update individual error and variable importance
  # create data frames to merge
  # variable importance - merge by 'var'
  var.imp = data.frame(var=colnames(trainData[,-res.ind]))
  # oob prediction - row names as ind, merge by 'ind'
  oob.pred = data.frame(ind=as.numeric(rownames(trainData)))  
  oob.res = data.frame(res=trainData[,res.ind])
  # test prediction - row names as ind, merge by 'ind'
  if(!is.null(testData)) {
    test.pred = data.frame(ind=as.numeric(rownames(testData)))
    test.res = data.frame(res=testData[,res.ind])
  } else {
    test.pred = NULL
    test.res = NULL
  } 
  require(rpart)
  for(i in 1:ntree) {
    # create in bag and out of bag sample
    bag = sample(nrow(trainData), size=nrow(trainData), replace=TRUE)
    inbag = trainData[bag,]
    outbag = trainData[-bag,]
    # fit model
    mod = rpart(formula=formula, data=inbag, control=rpart.control(cp=0))
    # set helper variables
    colname = paste("s",i,sep=".")
    pred.type = ifelse(class(trainData[,res.ind])=="factor","class","vector")
    # merge variable importance
    imp = data.frame(names(mod$variable.importance), mod$variable.importance)
    colnames(imp) = c("var", colname)
    var.imp = merge(var.imp, imp, by="var", all=TRUE)
    # fit data
    fit = function(obj, data, type, name) {
      fit.mod = predict(object=obj, newdata=data, type=type)
      fit.df = data.frame(as.numeric(names(fit.mod)), fit.mod, row.names=NULL)
      colnames(fit.df) = c("ind", name)
      fit.df
    }    
    # merge oob/test prediction
    oob.fit = fit(mod, data=outbag, type=pred.type, name=colname)
    oob.pred = merge(oob.pred, oob.fit, by="ind", all=TRUE)
    if(!is.null(testData)) {
      test.fit = fit(mod, data=testData, type=pred.type, name=colname)
      test.pred = merge(test.pred, test.fit, by="ind", all=TRUE)      
    }
  }
  # adjust outcomes
  rownames(var.imp) = var.imp[,1]
  var.imp = var.imp[,2:ncol(var.imp)]
  oob.pred = oob.pred[,2:ncol(oob.pred)]
  if(!is.null(testData)) {
    test.pred = test.pred[,2:ncol(test.pred)]
  }
  
  # create outcome as a list
  result=list(ntree=ntree, var.imp = var.imp
              ,oob.res=oob.res, oob.pred=oob.pred
              ,test.res=test.res, test.pred=test.pred)
  class(result) = c("rpartbgg")
  result
}

#
# comBGG() returns consolidated variable importance measures
#
# Usage
#
# last modified on Mar 19, 2015
#
comBGG = function(...) {
  # add rpart objects in a list
  bgglist = list(...)
  # extract variable importance
  var.imp = lapply(bgglist, function(x) x$var.imp)
  # combine and sum by row
  var.imp = do.call("cbind", var.imp)
  var.imp = apply(var.imp, 1, sum, na.rm=TRUE)
  var.imp
}



