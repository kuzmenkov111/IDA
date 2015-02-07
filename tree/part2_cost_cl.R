set.seed(1)
levels <- c("normal", "overweight")
data = sample(levels,100,replace=T)
prediction = sample(levels,100,replace=T)
cm = table(Actual=data,Predicted=prediction)
cm


getUpdatedCM = function(cm) {
  if(class(cm)=="table") {
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
    rownames(cmUp) = c(paste("Actl",rownames(cm),sep=": "),"Use Error")
    colnames(cmUp) = c(paste("Pred",colnames(cm),sep=": "),"Model Error")  
    cmUp
  }
  else {
    message("Please enter an object of the 'table' class")
  }
}