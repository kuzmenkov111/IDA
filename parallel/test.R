install.packages("snow")

library(snow)
cl = makeCluster(4, type="SOCK")
stopCluster(cl)

library(MASS)
result = kmeans(Boston, centers=4, nstart=100)
res0 = result

results = lapply(rep(25,4), function(nstart) kmeans(Boston, 4, nstart=nstart) )
i = sapply(results, function(result) result$tot.withinss)
result = results[[which.min(i)]]

cl = makeCluster(4, type="SOCK")
# data should be accessible in each worker, evaluate library(MASS)
ignore = clusterEvalQ(cl, {library(MASS); NULL})
results = clusterApply(cl, rep(25,4), function(nstart) kmeans(Boston, 4, nstart=nstart))
i = sapply(results, function(result) result$tot.withinss)
result = results[[which.min(i)]]
stopCluster(cl)

## foreach
library(foreach)
# foreach with %do% is used to execute an R expression repeatedly
# and return the results in some data structure or object, which is a list by default
x = foreach(i=1:3) %do% sqrt(i)
x = foreach(a=1:3, b=rep(10,3)) %do% (a+b)
x = foreach(a=1:3, b=rep(10,3)) %do% {
  a+b
}
# return a list of two elements
x = foreach(a=1:1000, b=rep(10,2)) %do% (a+b)

# .combine - c, +, -, rbind, cbind, sum
# .multicombine, .maxcombine
# .inorder - important if parallel
x = foreach(i=1:3, .combine="c") %do% exp(i)
x = foreach(i=1:3, .combine="cbind") %do% rnorm(4)
x = foreach(i=1:3, .combine="c") %do% rnorm(4)
x = foreach(i=1:4, .combine='cbind') %do% {
  rand = rnorm(4)
  sum(rand)/length(rand)
}
## foreach with iterators
library(iterators)
x = foreach(a = 1:3, .combine='cbind') %do% rnorm(4)
# iterators allow the data to be generated on-the-fly, as it is needed by your operations
# rather than requiring all of the data to be generated at the beginning.
x = foreach(a=irnorm(4, count=3), .combine='cbind') %do% a
set.seed(123)
x = foreach(a=irnorm(4, count=1000), .combine='+') %do% a
x = foreach(a=icount(1000), .combine='+') %do% rnorm(4)
## parallel execution
set.seed(123)
x = matrix(runif(500), 100)
y = gl(2, 50)
library(randomForest)
rf1 = foreach(ntree=rep(250, 4), .combine=combine) %do% randomForest(x, y, ntree=ntree)
rf2 = foreach(ntree=rep(250, 4), .combine=combine, .packages='randomForest') %dopar% randomForest(x, y, ntree=ntree)

applyKernel = function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  ans = vector("list", d2)
  for(i in 1:d2) {
    tmp = FUN(array(newX[,i], d.call, dn.call))
    if(!is.null(tmp)) ans[[i]] = tmp
  }
  ans
}
applyKernel = function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  foreach(i=1:d2) %dopar%
    FUN(array(newX[,i], d.call, dn.call), ...)
}
applyKernel = function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  foreach(x=iter(newX, by='col')) %dopar%
    FUN(array(x, d.call, dn.call), ...)
}

iblkcol <- function(a, chunks) {
  n <- ncol(a)
  i <- 1  
  nextElem <- function() {
    if (chunks <= 0 || n <= 0) stop('StopIteration')
    m <- ceiling(n / chunks)
    r <- seq(i, length=m)
    i <<- i + m
    n <<- n - m
    chunks <<- chunks - 1
    a[,r, drop=FALSE]
  }  
  structure(list(nextElem=nextElem), class=c('iblkcol', 'iter'))
}
nextElem.iblkcol <- function(obj) obj$nextElem()
applyKernel = function(newX, FUN, d2, d.call, dn.call=NULL, ...) {
  foreach(x=iblkcol(newX, 3), .combine='c', .packages='foreach') %dopar% {
    foreach(i=1:ncol(x)) %do% FUN(array(x[,i], d.call, dn.call), ...)
  }
}
applyKernel(matrix(1:16, 4), mean, 4, 4)










