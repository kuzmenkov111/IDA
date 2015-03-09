dbindex.avg <- function(clustering, centers, data, metric="euclidean", stand=FALSE)
{ mean(dbindex.cluster(clustering, centers, data, metric, stand)) }

  # training set average Davies-Bouldin index
dbindex.avg(i.pam2.euc$clustering, i.pam2.euc$medoids, i.std.train[,-5])
dbindex.avg(i.pam3.euc$clustering, i.pam3.euc$medoids, i.std.train[,-5])
dbindex.avg(i.pam5.euc$clustering, i.pam5.euc$medoids, i.std.train[,-5])
dbindex.avg(i.pam7.euc$clustering, i.pam7.euc$medoids, i.std.train[,-5])

dbindex.avg(i.pam2.man$clustering, i.pam2.euc$medoids, i.std.train[,-5],
            metric="manhattan")
dbindex.avg(i.pam3.man$clustering, i.pam3.euc$medoids, i.std.train[,-5],
            metric="manhattan")
dbindex.avg(i.pam5.man$clustering, i.pam5.euc$medoids, i.std.train[,-5],
            metric="manhattan")
dbindex.avg(i.pam7.man$clustering, i.pam7.euc$medoids, i.std.train[,-5],
            metric="manhattan")

  # test set average Davies-Bouldin index
dbindex.avg(i.pam2.euc.pred, i.pam2.euc$medoids, i.std.test[,-5])
dbindex.avg(i.pam3.euc.pred, i.pam3.euc$medoids, i.std.test[,-5])
dbindex.avg(i.pam5.euc.pred, i.pam5.euc$medoids, i.std.test[,-5])
dbindex.avg(i.pam7.euc.pred, i.pam7.euc$medoids, i.std.test[,-5])

dbindex.avg(i.pam2.man.pred, i.pam2.euc$medoids, i.std.test[,-5], metric="manhattan")
dbindex.avg(i.pam3.man.pred, i.pam3.euc$medoids, i.std.test[,-5], metric="manhattan")
dbindex.avg(i.pam5.man.pred, i.pam5.euc$medoids, i.std.test[,-5], metric="manhattan")
dbindex.avg(i.pam7.man.pred, i.pam7.euc$medoids, i.std.test[,-5], metric="manhattan")
