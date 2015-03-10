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
myMean = function(x) { sum(x)/length(x) }
x = foreach(i=1:4, .combine='c') %do% {
  rand = rnorm(4)
  sum(rand)/length(rand)
}










