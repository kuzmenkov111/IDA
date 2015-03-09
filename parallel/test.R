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

