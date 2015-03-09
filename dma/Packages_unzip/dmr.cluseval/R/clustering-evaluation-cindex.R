cindex <- function(clustering, data, metric="euclidean", stand=FALSE)
{
  clusters <- unique(clustering)
  dm <- as.matrix(daisy(data, metric, stand))
  dm[lower.tri(dm)] <- diag(dm) <- NA
  sdm <- sort(dm)
  cc <- table(clustering)
  m <- sum(cc*(cc-1)/2)

  s <- sum(sapply(clusters,
                  function(d) sum(dm[clustering==d,clustering==d], na.rm=TRUE)))
  smin <- sum(sdm[1:m])
  smax <- sum(sdm[(length(sdm)-m+1):length(sdm)])
  (s-smin)/(smax-smin)
}


if (FALSE)
{

  # training set C index
cindex(i.pam2.euc$clustering, i.std.train[,-5])
cindex(i.pam3.euc$clustering, i.std.train[,-5])
cindex(i.pam5.euc$clustering, i.std.train[,-5])
cindex(i.pam7.euc$clustering, i.std.train[,-5])

cindex(i.pam2.man$clustering, i.std.train[,-5], metric="manhattan")
cindex(i.pam3.man$clustering, i.std.train[,-5], metric="manhattan")
cindex(i.pam5.man$clustering, i.std.train[,-5], metric="manhattan")
cindex(i.pam7.man$clustering, i.std.train[,-5], metric="manhattan")

  # test set C index
cindex(i.pam2.euc.pred, i.std.test[,-5])
cindex(i.pam3.euc.pred, i.std.test[,-5])
cindex(i.pam5.euc.pred, i.std.test[,-5])
cindex(i.pam7.euc.pred, i.std.test[,-5])

cindex(i.pam2.man.pred, i.std.test[,-5], metric="manhattan")
cindex(i.pam3.man.pred, i.std.test[,-5], metric="manhattan")
cindex(i.pam5.man.pred, i.std.test[,-5], metric="manhattan")
cindex(i.pam7.man.pred, i.std.test[,-5], metric="manhattan")

}
