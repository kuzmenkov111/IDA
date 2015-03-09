dbindex <- function(clustering, centers, data, metric="euclidean", stand=FALSE)
{
  clusters <- sort(unique(clustering))
  ds <- as.matrix(daisy(rbind(data, centers), metric, stand))

  to.center <- sapply(clusters, function(d) mean(ds[clustering==d,nrow(data)+d]))
  between.centers <- ds[(nrow(data)+1):(nrow(data)+length(clusters)),
                        (nrow(data)+1):(nrow(data)+length(clusters))]
  diag(between.centers) <- NA

  `dimnames<-`(outer(clusters, clusters, Vectorize(function(d1, d2)
                                                   (to.center[d1]+to.center[d2])/
                                                     between.centers[d1,d2])),
               list(clusters, clusters))
}

dbindex.cluster <- function(clustering, centers, data,
                            metric="euclidean", stand=FALSE)
{ apply(dbindex(clustering, centers, data, metric, stand), 1, max, na.rm=TRUE) }


if (FALSE)
{

  # training set Davies-Bouldin index for cluster pairs
dbindex(i.pam3.euc$clustering, i.pam3.euc$medoids, i.std.train[,-5])

  # training set Davies-Bouldin index
dbindex.cluster(i.pam2.euc$clustering, i.pam2.euc$medoids, i.std.train[,-5])
dbindex.cluster(i.pam3.euc$clustering, i.pam3.euc$medoids, i.std.train[,-5])
dbindex.cluster(i.pam5.euc$clustering, i.pam5.euc$medoids, i.std.train[,-5])
dbindex.cluster(i.pam7.euc$clustering, i.pam7.euc$medoids, i.std.train[,-5])

dbindex.cluster(i.pam2.man$clustering, i.pam2.euc$medoids, i.std.train[,-5],
                metric="manhattan")
dbindex.cluster(i.pam3.man$clustering, i.pam3.euc$medoids, i.std.train[,-5],
                metric="manhattan")
dbindex.cluster(i.pam5.man$clustering, i.pam5.euc$medoids, i.std.train[,-5],
                metric="manhattan")
dbindex.cluster(i.pam7.man$clustering, i.pam7.euc$medoids, i.std.train[,-5],
                metric="manhattan")

  # test set Davies-Bouldin index
dbindex.cluster(i.pam2.euc.pred, i.pam2.euc$medoids, i.std.test[,-5])
dbindex.cluster(i.pam3.euc.pred, i.pam3.euc$medoids, i.std.test[,-5])
dbindex.cluster(i.pam5.euc.pred, i.pam5.euc$medoids, i.std.test[,-5])
dbindex.cluster(i.pam7.euc.pred, i.pam7.euc$medoids, i.std.test[,-5])

dbindex.cluster(i.pam2.man.pred, i.pam2.euc$medoids, i.std.test[,-5],
                metric="manhattan")
dbindex.cluster(i.pam3.man.pred, i.pam3.euc$medoids, i.std.test[,-5],
                metric="manhattan")
dbindex.cluster(i.pam5.man.pred, i.pam5.euc$medoids, i.std.test[,-5],
                metric="manhattan")
dbindex.cluster(i.pam7.man.pred, i.pam7.euc$medoids, i.std.test[,-5],
                metric="manhattan")

}
