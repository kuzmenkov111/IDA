## medoid for data with respect to dissimilarity matrix dm
medoid <- function(data, dm)
{
  data[which.min(colMeans(dm)),]
}

## k-medoids center adjustment
k.centers.adjust.medoid <- function(clustering, data, k, diss, dm)
{
  do.call(rbind, lapply(1:k, function(d)
                             medoid(data[clustering==d,],
                                    as.matrix(dm)[clustering==d, clustering==d])))
}


if (FALSE)
{

  # k-medoids clustering
w.kmedoids <- k.centers(wcl.std, 3, adjust=k.centers.adjust.medoid)
w.kmedoids$centers

i.kmedoids <- k.centers(i.std.train[,-5], 3, adjust=k.centers.adjust.medoid)
g.kmedoids <- k.centers(g.std.train[,-10], 7, adjust=k.centers.adjust.medoid)

  # k-medoids prediction
w.kmedoids$clustering
predict(w.kmedoids, wcl.std)

i.pred.kmedoids <- predict(i.kmedoids, i.std.test[,-5])
g.pred.kmedoids <- predict(g.kmedoids, g.std.test[,-10])

  # clusters vs. classes on the training set
table(i.kmedoids$clustering, i.std.train$Species)
table(g.kmedoids$clustering, g.std.train$Type)

  # clusters vs. classes on the test set
table(predict(i.kmedoids, i.std.test[,-5]), i.std.test$Species)
table(predict(g.kmedoids, g.std.test[,-10]), g.std.test$Type)

  # attribute distribution within clusters for the Iris data
par(mfrow=c(2, 2))
for (attr in names(i.std.train)[1:4])
  boxplot(i.std.train[[attr]]~i.kmedoids$clustering, main=attr)

}
