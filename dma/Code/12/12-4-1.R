## k-medians center adjustment
k.centers.adjust.median <- function(clustering, data, k, diss, dm)
{
  do.call(rbind, lapply(1:k, function(d) attr.mm(data[clustering==d,], mc=median)))
}

  # k-medians clustering
w.kmedians <- k.centers(wcl.std, 3, adjust=k.centers.adjust.median)
w.kmedians$centers

i.kmedians <- k.centers(i.std.train[,-5], 3, adjust=k.centers.adjust.median)
g.kmedians <- k.centers(g.std.train[,-10], 7, adjust=k.centers.adjust.median)

  # k-medians prediction
w.kmedians$clustering
predict(w.kmedians, wcl.std)

i.pred.kmedians <- predict(i.kmedians, i.std.test[,-5])
g.pred.kmedians <- predict(g.kmedians, g.std.test[,-10])

  # clusters vs. classes on the training set
table(i.kmedians$clustering, i.std.train$Species)
table(g.kmedians$clustering, g.std.train$Type)

  # clusters vs. classes on the test set
table(predict(i.kmedians, i.std.test[,-5]), i.std.test$Species)
table(predict(g.kmedians, g.std.test[,-10]), g.std.test$Type)

  # attribute distribution within clusters for the Iris data
par(mfrow=c(2, 2))
for (attr in names(i.std.train)[1:4])
  boxplot(i.std.train[[attr]]~i.kmedoids$clustering, main=attr)
