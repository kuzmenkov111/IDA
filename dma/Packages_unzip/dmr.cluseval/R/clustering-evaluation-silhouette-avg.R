silwidth.avg <- function(clustering, data, metric="euclidean", stand=FALSE)
{
  clusters <- unique(clustering)
  mean(unlist(sapply(clusters,
                     function(d) silwidth(clustering, d, data, metric, stand))))
}


if (FALSE)
{

  # training set average silhouette width
silwidth.avg(i.pam2.euc$clustering, i.std.train[,-5])
silwidth.avg(i.pam3.euc$clustering, i.std.train[,-5])
silwidth.avg(i.pam5.euc$clustering, i.std.train[,-5])
silwidth.avg(i.pam7.euc$clustering, i.std.train[,-5])

silwidth.avg(i.pam2.man$clustering, i.std.train[,-5], metric="manhattan")
silwidth.avg(i.pam3.man$clustering, i.std.train[,-5], metric="manhattan")
silwidth.avg(i.pam5.man$clustering, i.std.train[,-5], metric="manhattan")
silwidth.avg(i.pam7.man$clustering, i.std.train[,-5], metric="manhattan")

  # test set average silhouette width
silwidth.avg(i.pam2.euc.pred, i.std.test[,-5])
silwidth.avg(i.pam3.euc.pred, i.std.test[,-5])
silwidth.avg(i.pam5.euc.pred, i.std.test[,-5])
silwidth.avg(i.pam7.euc.pred, i.std.test[,-5])

silwidth.avg(i.pam2.man.pred, i.std.test[,-5], metric="manhattan")
silwidth.avg(i.pam3.man.pred, i.std.test[,-5], metric="manhattan")
silwidth.avg(i.pam5.man.pred, i.std.test[,-5], metric="manhattan")
silwidth.avg(i.pam7.man.pred, i.std.test[,-5], metric="manhattan")

}
