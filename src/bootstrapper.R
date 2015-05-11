<<<<<<< HEAD
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
=======
library(rpart)

## data
set.seed(1237)
x <- matrix(runif(500), 100)
y <- runif(100)

data <- as.data.frame(cbind(y, x))
names(data) <- c("y","x1","x2","x3","x4","x5")

## functional
functional <- function(f, ...) {
  f(...)
}

functional(f = lm, formula = y ~ ., data = data)
functional(f = glm, formula = y ~ ., data = data, family = gaussian)
functional(f = rpart, formula = y ~ ., data = data, control = rpart.control(cp = 0))

## closure + functional
ntrial <- 100
bootstrapper <- function(formula, data, ntrial) {
  # check if response is found
  res.name = gsub(" ","",unlist(strsplit(formula,split="~"))[[1]])
  res.ind = match(res.name, colnames(data))

  function(f, ...) {
    lapply(1:ntrial, function(i) {
      # do bootstrap
      bag <- sample(nrow(data), size = nrow(data), replace = TRUE)
      model <- f(formula = formula, data = data[bag,], ...)
      fitted <- predict(model)
      actual <- data[bag, res.ind]
      error <- if(class(actual) == "numeric") {
        sqrt(sum((fitted - actual)^2) / length(actual))
      } else {
        1 - diag(table(actual, fitted)) / sum(table(actual, fitted))
      }
      
      list(model = model, error = error)      
>>>>>>> 7eb5120eac57e100c22a0844f8b3c5a1a82ac14b
    })
  }
}

<<<<<<< HEAD


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
=======
boot_configure <- bootstrapper(formula = "y ~ .", data = data, ntrial = ntrial)

set.seed(1237)
boot_lm <- boot_configure(lm)
mean(do.call(c, lapply(boot_lm, function(x) x$error)))

set.seed(1237)
boot_cart <- boot_configure(rpart)
mean(do.call(c, lapply(boot_cart, function(x) x$error)))

>>>>>>> 7eb5120eac57e100c22a0844f8b3c5a1a82ac14b
