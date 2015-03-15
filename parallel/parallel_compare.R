library(parallel)
library(iterators)
library(foreach)
library(doParallel)
library(randomForest)

## k-means clustering
# parallel
split = detectCores()
eachStart = 25

cl = makeCluster(split)
init = clusterEvalQ(cl, { library(MASS); NULL })
results = parLapplyLB(cl
                      ,rep(eachStart, split)
                      ,function(nstart) kmeans(Boston, 4, nstart=nstart))
withinss = sapply(results, function(result) result$tot.withinss)
result = results[[which.min(withinss)]]
stopCluster(cl)

result$tot.withinss

# foreach
rm(list = ls())
split = detectCores()
eachStart = 25
# set up iterators
iters = iter(rep(eachStart, split))
# set up combine function
comb = function(res1, res2) {
  if(res1$tot.withinss < res2$tot.withinss) res1 else res2
}

cl = makeCluster(split)
registerDoParallel(cl)
result = foreach(nstart=iters, .combine="comb", .packages="MASS") %dopar%
  kmeans(Boston, 4, nstart=nstart)
stopCluster(cl)

result$tot.withinss

## Random forest
# parallel
rm(list = ls())
set.seed(1237)
x = matrix(runif(500), 100)
y = gl(2,50)
split = detectCores()
eachTrees = 250
# define function to fit random forest given predictors and response
# data has to be sent to workers using this function
rf = function(ntree, pred, res) {
  randomForest(pred, res, ntree=ntree)
}

cl = makeCluster(split)
clusterSetRNGStream(cl, iseed=1237)
init = clusterEvalQ(cl, { library(randomForest); NULL })
results = parLapplyLB(cl, rep(eachTrees, split), rf, pred=x, res=y)
result = do.call("combine", results)
stopCluster(cl)

cm = table(data.frame(actual=y, fitted=result$predicted))
cm

# foreach
rm(list = ls())
set.seed(1237)
x = matrix(runif(500), 100)
y = gl(2,50)
split = detectCores()
eachTrees = 250
# set up iterators
iters = iter(rep(eachTrees, split))

cl = makeCluster(split)
clusterSetRNGStream(cl, iseed=1237)
registerDoParallel(cl)
result = foreach(ntree=iters, .combine=combine, .packages="randomForest") %dopar%
  randomForest(x, y, ntree=ntree)
stopCluster(cl)

cm = table(data.frame(actual=y, fitted=result$predicted))
cm

## Bagging
# parallel



# foreach


