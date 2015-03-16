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
  if(!file.exists("src/mlUtils.R"))
    stop("utility functions should be sourced")
  
  ## import utility functions
  source("src/mlUtils.R")
  # fit model
  mod = rpart(formula=formula, data=trainData, control=rpart.control(cp=0))  
}


#
# bestParam returns a matrix of least and best cp values given cp table of rpart object
# 1 standard error rule is applied for best cp value cp, error and errStd are returned
#
# Usage
# require(rpart)
# set.seed(125)
# res = as.factor(sample(c("T","F"),100,replace=TRUE))
# pred = as.data.frame(matrix(rnorm(500), ncol=5))
# data = cbind(pred, res)
# mod = rpart(res ~ ., data=data, control=rpart.control(cp=0))
# bestParam(mod$cptable,"CP","xerror","xstd")
#
# last modified on Mar 16, 2015
#

bestParam = function(tbl,param,error,errStd, ...) {
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

#
# updateCM aims to extend a confusion matrix 
# by adding model errors to the last column and use errors to the last row.
# Both square and non-square matrices are supported
#
# Usage
# set.seed(1)
# actual = ifelse(rnorm(100,0,1)<0,"low","high")
# fitted = ifelse(rnorm(100,1,5)<0,"low","high")
# updateCM(table(actual,fitted))
#
# last modified on Feb 14, 2015
#

updateCM = function(cm, type="Pred") {
  if(class(cm)=="table") {
    if(length(dim(cm)) == 2) {
      if(nrow(cm) > 0) {
        # row wise misclassification
        modelError = function(cm) {
          errors = c()
          # if(square) else(non-square matrix)
          if(nrow(cm)==ncol(cm)) {
            for(i in 1:nrow(cm)) {
              err = (sum(cm[i,]) - sum(cm[i,i])) / sum(cm[i,])
              errors = c(errors,err)
            }
            errors = c(errors,(1-sum(diag(cm))/sum(cm)))             
          } else {
            df = as.data.frame(cm,stringsAsFactors = FALSE)
            colnames(df) = c("row","col","freq")
            for(i in 1:nrow(cm)) {
              freq = tryCatch({
                sum(subset(df,subset=row==rownames(cm)[i] & col==rownames(cm)[i],select=freq))
              },
              error=function(cond) { return(0) }
              )
              err = (sum(cm[i,]) - freq) / sum(cm[i,])
              errors = c(errors,err)
            }            
            freq = tryCatch({
              sum(subset(df,subset=row==col,select=freq))
            },
            error=function(cond) { return(0) }
            )
            errors = c(errors,(1-freq/sum(cm)))            
          } 
          errors
        }    
        # column wise misclassification
        useError = function(cm) {
          errors = c()
          # if(square) else(non-square matrix)
          if(nrow(cm)==ncol(cm)) {
            for(i in 1:ncol(cm)) {
              err = (sum(cm[,i]) - sum(cm[i,i])) / sum(cm[,i])
              errors = c(errors,err)             
            }        
          } else {
            for(i in 1:ncol(cm)) {
              freq = tryCatch({
                sum(subset(df,subset=row==colnames(cm)[i] & col==colnames(cm)[i],select=freq))
              },
              error=function(cond) { return(0) }
              )
              err = (sum(cm[,i]) - freq) / sum(cm[,i])
              errors = c(errors,err)
            }
          }
          errors
        }
        
        # use error added to the last row
        cmUp = rbind(cm,as.table(useError(cm)))
        # model error added to the last column
        cmUp = cbind(cmUp,as.matrix(modelError(cm)))
        rownames(cmUp) = c(paste("actual",rownames(cm),sep=": "),"Use Error")
        colnames(cmUp) = c(paste(type,colnames(cm),sep=": "),"Model Error")  
        round(cmUp,2)        
      } else {
        message("Table has no row")
      }
    }
    else {
      message("Enter a table with 2 variables")
    }
  }
  else {
    message("Enter an object of the 'table' class")
  }
}

#
# regCM() produces a confusion matrix for a continuous response  
# where quantile values of actual data split both actual and fitted response
# Note that it depends on updateCM() and do not include 0 and 1 in probs
#
# Usage
# set.seed(1)
# actual = rnorm(100,0,1)
# fitted = rnorm(100,1,5)
# probs = seq(0.25,0.75,0.25)
# regCM(actual,fitted,probs)
#
# last modified on Feb 14, 2015
#

regCM = function(actual, fitted, probs, type="Pred", ...) {
  if(length(actual[is.na(actual)]) + length(fitted[is.na(fitted)]) > 0) {
    message("Currently NA values are not supported")
  } else {
    probs = sort(round(probs,2))
    conv = function(data,ref,probs) {
      # ref is reference to produce quantile values
      outs = c()
      if(length(probs) == 1) {
        lower = length(data[data<=quantile(ref,probs[1])])
        upper = length(data[data>quantile(ref,probs[1])])
        outs = c(outs,rep(paste0(probs[1]*100,"%-"),lower)
                 ,rep(paste0(probs[1]*100,"%+"),upper))
      } else {
        lower = length(data[data<=quantile(ref,probs[1])])
        outs = c(outs,rep(paste0(probs[1]*100,"%-"),lower))
        for(i in 2:length(probs)) {
          lower = length(data[data<=quantile(ref,probs[i])]) - length(data[data<=quantile(ref,probs[i-1])])
          outs = c(outs,rep(paste0(probs[i]*100,"%-"),lower))
        }
        upper = length(data[data>quantile(ref,probs[length(probs)])])
        outs = c(outs,rep(paste0(probs[length(probs)]*100,"%+"),upper))
      }
      outs
    }
    updateCM(table(conv(actual,actual,probs),conv(fitted,actual,probs)),type)
  }
}
