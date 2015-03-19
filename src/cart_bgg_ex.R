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

## data
require(ISLR)
data(Carseats)

set.seed(12357)
bgg = cartBGG(formula="Sales ~ .", Carseats, ntree=10)

comBGG(bgg)