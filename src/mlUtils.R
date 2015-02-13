#
# updateCM aims to extend a confusion matrix 
# by adding model errors to the last column and use errors to the last row.
#
# Usage
# updateCM(cm)
#
# last modified on Feb 13, 2015
#

updateCM = function(cm, type="Pred") {
  if(class(cm)=="table") {
    if(nrow(cm) > 0 & length(dim(cm)) > 1) {
      # row wise misclassification
      modelError = function(cm) {
        errors = c()
        for(i in 1:nrow(cm)) {
          err = (sum(cm[i,]) - sum(cm[i,i])) / sum(cm[i,])
          errors = c(errors,err)
        }
        errors = c(errors,(1-sum(diag(cm))/sum(cm)))  
        errors
      }    
      # column wise misclassification
      useError = function(cm) {
        errors = c()
        for(i in 1:ncol(cm)) {
          err = (sum(cm[,i]) - sum(cm[i,i])) / sum(cm[,i])
          errors = c(errors,err)
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
    }
    else {
      message("Enter a square table")
    }
  }
  else {
    message("Enter an object of the 'table' class")
  }
}

#
# regCM produces a confusion matrix for a continuous response  
# where quantile values of actual data split both actual and fitted response
#
# Usage
# set.seed(1)
# actual = rnorm(100,0,1)
# fitted = rnorm(100,1,5)
# probs = c(0.2)
# regCM(actual,fitted,probs)
#
# last modified on Feb 13, 2015
#

regCM = function(actual, fitted, probs, type="Pred", ...) {
  if(length(actual[is.na(actual)]) + length(fitted[is.na(fitted)]) > 0) {
    message("Currently NA values are not supported")
  } else {
    probs = sort(round(probs,2))  
    conv = function(data,ref,probs) {
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
          lower = length(data[data<=quantile(ref,probs[i])]) - length(data[data<quantile(ref,probs[i-1])])
          outs = c(outs,rep(paste0(probs[i]*100,"%-"),lower))
        }
        upper = length(data[data>quantile(ref,probs[length(probs)])])
        outs = c(outs,rep(paste0(probs[length(probs)]*100,"%+"),upper))
      }
      outs
    }
    updateCM(table(conv(actual,actual,probs),conv(fitted,actual,probs)))    
  }
}

#
# bestParam select best single tunning parameter given measure, error and std of error
# 1 standard error is applied and only 'cptable' of rpart() is tested.
#
# Usage
# require(ISLR)
# require(rpart)
# set.seed(1)
# mod = rpart(Sales ~ ., data=Carseats, control=rpart.control(cp=0))
# bestParam(mod$cptable,"CP","xerror","xstd")
#
# result:
# CP     nsplit  rel error     xerror       xstd 
# 0.01402704 9.00000000 0.42781645 0.59360492 0.03907472
#
# last modified on Feb 14, 2015
#

bestParam = function(data,measure,error,errStd,isDesc=TRUE,...) {
  # convert name to index
  ind = function(name, df) { grep(name, colnames(data)) }
  measure = ind(measure,df)
  error = ind(error, df)
  errStd = ind(errStd, df)
  # get min error
  pick = c(1,data[1,error],data[1,errStd])
  from = ifelse(isDesc,1,nrow(data))
  to = ifelse(isDesc,nrow(data),1)
  by = ifelse(isDesc,1,-1)
  for(i in seq(from,to,by)) {
    if(data[i,error]<=pick[2]) pick=c(i,data[i,error],data[i,errStd])
  }
  # select best param
  df = data[data[,error]<=pick[2]+pick[3],]
  if(isDesc) df[1,] else df[nrow(df),]
}