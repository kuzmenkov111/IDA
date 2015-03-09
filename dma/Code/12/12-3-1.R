## attribute value means, medians, or modes
attr.mm <- function(data, mc=mean, md=modal)
{
  data.frame(`names<-`(lapply(data, function(v)
                              if (is.numeric(v)) mc(v) else md(v)),
                       names(data)))
}

## k-means center adjustment
k.centers.adjust.mean <- function(clustering, data, k, diss, dm)
{
  do.call(rbind, lapply(1:k, function(d) attr.mm(data[clustering==d,])))
}

  # k-means clustering
w.kmeans <- k.centers(wcl.std, 3, adjust=k.centers.adjust.mean)
w.kmeans$centers

i.kmeans <- k.centers(i.std.train[,-5], 3, adjust=k.centers.adjust.mean)
g.kmeans <- k.centers(g.std.train[,-10], 7, adjust=k.centers.adjust.mean)

  # k-means prediction
w.kmeans$clustering
predict(w.kmeans, wcl.std)

i.pred.kmeans <- predict(i.kmeans, i.std.test[,-5])
g.pred.kmeans <- predict(g.kmeans, g.std.test[,-10])

  # clusters vs. classes on the training set
table(i.kmeans$clustering, i.std.train$Species)
table(g.kmeans$clustering, g.std.train$Type)

  # clusters vs. classes on the test set
table(predict(i.kmeans, i.std.test[,-5]), i.std.test$Species)
table(predict(g.kmeans, g.std.test[,-10]), g.std.test$Type)

  # attribute distribution within clusters for the Iris data
par(mfrow=c(2, 2))
for (attr in names(i.std.train)[1:4])
  boxplot(i.std.train[[attr]]~i.kmeans$clustering, main=attr)
