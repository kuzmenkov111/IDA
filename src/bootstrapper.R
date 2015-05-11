power <- function(exponent, ...) {
  function(x) {
    x ^ exponent
  }
}

power(2)(3)


bootstrapper <- function(f, ...) {
  function(formula, data, ntrial) {
    res.name <- gsub(" ","", unlist(strsplit(formual, split="~")))[[1]]
    res.ind <- match(res.name, colnames(data))
    
    lapply(1:ntrial, function(x) {
      bag.ind <- sample(nrow(data), size = nrow(data), replace = TRUE)
      inbag <- data[bag.ind,]
      outbag <- data[-bag.ind,]
      
      mod <- f(formula, data, ...)
    })
  }
}



# update variable importance measure of bagged trees
cartBGG = function(formula, trainData, ntree=1, ...) {
  # extract response name and index
  res.name = gsub(" ","",unlist(strsplit(formula,split="~"))[[1]])
  res.ind = match(res.name, colnames(trainData))
  
  # variable importance - merge by 'var'
  var.imp = data.frame(var=colnames(trainData[,-res.ind]))
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
  }
  # adjust outcome
  rownames(var.imp) = var.imp[,1]
  var.imp = var.imp[,2:ncol(var.imp)]
  
  # create outcome as a list
  result=list(var.imp = var.imp)
  class(result) = c("rpartbgg")
  result
}