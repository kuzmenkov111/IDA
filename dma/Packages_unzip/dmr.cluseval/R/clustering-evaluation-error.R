clustclas <- function(train.clustering, train.classes,
                      eval.clustering=train.clustering)
{
  clusters <- unique(train.clustering)
  labels <-
    sapply(clusters,
         function(d)
         levels(train.classes)[which.max(pdisc(train.classes[train.clustering==d]))])
  factor(labels[eval.clustering], levels=levels(train.classes))
}


if (FALSE)
{

  # training set error
err(clustclas(i.pam2.euc$clustering, i.std.train[,5]), i.std.train[,5])
err(clustclas(i.pam3.euc$clustering, i.std.train[,5]), i.std.train[,5])
err(clustclas(i.pam5.euc$clustering, i.std.train[,5]), i.std.train[,5])
err(clustclas(i.pam7.euc$clustering, i.std.train[,5]), i.std.train[,5])

err(clustclas(i.pam2.man$clustering, i.std.train[,5]), i.std.train[,5])
err(clustclas(i.pam3.man$clustering, i.std.train[,5]), i.std.train[,5])
err(clustclas(i.pam5.man$clustering, i.std.train[,5]), i.std.train[,5])
err(clustclas(i.pam7.man$clustering, i.std.train[,5]), i.std.train[,5])

  # test set error
err(clustclas(i.pam2.euc$clustering, i.std.train[,5], i.pam2.euc.pred),
    i.std.test[,5])
err(clustclas(i.pam3.euc$clustering, i.std.train[,5], i.pam3.euc.pred),
    i.std.test[,5])
err(clustclas(i.pam5.euc$clustering, i.std.train[,5], i.pam5.euc.pred),
    i.std.test[,5])
err(clustclas(i.pam7.euc$clustering, i.std.train[,5], i.pam7.euc.pred),
    i.std.test[,5])

err(clustclas(i.pam2.man$clustering, i.std.train[,5], i.pam2.man.pred),
    i.std.test[,5])
err(clustclas(i.pam3.man$clustering, i.std.train[,5], i.pam3.man.pred),
    i.std.test[,5])
err(clustclas(i.pam5.man$clustering, i.std.train[,5], i.pam5.man.pred),
    i.std.test[,5])
err(clustclas(i.pam7.man$clustering, i.std.train[,5], i.pam7.man.pred),
    i.std.test[,5])

}
