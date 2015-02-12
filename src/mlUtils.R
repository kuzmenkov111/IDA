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

bestCP = function(data,cp,err,std,decreasing=TRUE,...) {
  # get index of each column
  ind = function(name, df) { grep(name, colnames(df)) }
  cpInd = ind(cp,df)
  errInd = ind(err, df)
  stdInd = ind(std, df)
  # reorder if necessary
  data = as.data.frame(data)
  data = data[order(data[,cpInd],decreasing=decreasing),]
  # create subset to apply 1-SE rule - cp, xerror, xstd if rpart
  df = data.frame(cp=data[2:nrow(data),cpInd])
  df$err = abs(diff(data[,errInd]))
  df$std = data[1:(nrow(data)-1),stdInd]
  #df = cbind(data[2:nrow(data),cpInd],abs(diff(data[,errInd])),data[1:(nrow(data)-1),stdInd])
  #colnames(df) <- c(cp,err,std)
  # take if diff(xerror) > xstd
  subDF = subset(df,df[,2]>df[,3])
  # if found, get the last row, otherwise take the first column
  if(nrow(subDF)>0) {
    subDF = df[nrow(subDF),]
  } else {
    subDF = data.frame(data[1,cpInd],data[1,errInd],data[1,stdInd])
    colnames(subDF) <- c(cp,err,std)
  }  
  subDF[1]
}

mod = rpart(Sales ~ ., data=Carseats, control=rpart.control(cp=0))
data = mod$cptable
bestCP(mod$cptable,"CP","xerror","xstd")

cbind(data[2:nrow(data),1],abs(diff(data[,4])),data[1:(nrow(data)-1),5])
nrow(data)