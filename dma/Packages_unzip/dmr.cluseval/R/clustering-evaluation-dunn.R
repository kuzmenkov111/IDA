dunn <- function(clustering, data, metric="euclidean", stand=FALSE)
{
  min(separation(clustering, data, metric, stand))/
    max(diameter(clustering, data, metric, stand))
}


if (FALSE)
{

  # training set Dunn index
dunn(i.pam2.euc$clustering, i.std.train[,-5])
dunn(i.pam3.euc$clustering, i.std.train[,-5])
dunn(i.pam5.euc$clustering, i.std.train[,-5])
dunn(i.pam7.euc$clustering, i.std.train[,-5])

dunn(i.pam2.man$clustering, i.std.train[,-5], metric="manhattan")
dunn(i.pam3.man$clustering, i.std.train[,-5], metric="manhattan")
dunn(i.pam5.man$clustering, i.std.train[,-5], metric="manhattan")
dunn(i.pam7.man$clustering, i.std.train[,-5], metric="manhattan")

  # test set Dunn index
dunn(i.pam2.euc.pred, i.std.test[,-5])
dunn(i.pam3.euc.pred, i.std.test[,-5])
dunn(i.pam5.euc.pred, i.std.test[,-5])
dunn(i.pam7.euc.pred, i.std.test[,-5])

dunn(i.pam2.man.pred, i.std.test[,-5], metric="manhattan")
dunn(i.pam3.man.pred, i.std.test[,-5], metric="manhattan")
dunn(i.pam5.man.pred, i.std.test[,-5], metric="manhattan")
dunn(i.pam7.man.pred, i.std.test[,-5], metric="manhattan")

}
