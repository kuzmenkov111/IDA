clusloglik <- function(train.clustering, train.data,
                       eval.clustering=train.clustering, eval.data=train.data)
{
  clusters <- unique(train.clustering)
  prob.d <- function(d) { sum(train.clustering==d)/nrow(train.data) }
  prob.avd <- function(a, v, d)
  {
    ifelse(is.numeric(v),
           dnorm(v, mmean(train.data[train.clustering==d,a],
                          m0=mean(train.data[,a])),
                 sqrt(mvar(train.data[train.clustering==d,a],
                           m0=mean(train.data[,a]), s02=var(train.data[,a])))),
           mest(sum(train.data[,a]==v & train.clustering==d),
                sum(train.clustering==d),
                nlevels(train.data[,a]), 1/nlevels(train.data[,a])))
  }

  sum(sapply(1:nrow(eval.data),
             function(i)
             log(sum(sapply(clusters,
                            function(d)
                            prob.d(d)*prod(mapply(function(a, v) prob.avd(a, v, d),
                                                  1:ncol(eval.data),
                                                  eval.data[i,])))))))
}

  # training set loglikelihood
clusloglik(i.pam2.euc$clustering, i.std.train[,-5])
clusloglik(i.pam3.euc$clustering, i.std.train[,-5])
clusloglik(i.pam5.euc$clustering, i.std.train[,-5])
clusloglik(i.pam7.euc$clustering, i.std.train[,-5])

clusloglik(i.pam2.man$clustering, i.std.train[,-5])
clusloglik(i.pam3.man$clustering, i.std.train[,-5])
clusloglik(i.pam5.man$clustering, i.std.train[,-5])
clusloglik(i.pam7.man$clustering, i.std.train[,-5])

  # test set loglikelihood
clusloglik(i.pam2.euc$clustering, i.std.train[,-5], i.pam2.euc.pred,
           i.std.test[,-5])
clusloglik(i.pam3.euc$clustering, i.std.train[,-5], i.pam3.euc.pred,
           i.std.test[,-5])
clusloglik(i.pam5.euc$clustering, i.std.train[,-5], i.pam5.euc.pred,
           i.std.test[,-5])
clusloglik(i.pam7.euc$clustering, i.std.train[,-5], i.pam7.euc.pred,
           i.std.test[,-5])

clusloglik(i.pam2.man$clustering, i.std.train[,-5], i.pam2.man.pred,
           i.std.test[,-5])
clusloglik(i.pam3.man$clustering, i.std.train[,-5], i.pam3.man.pred,
           i.std.test[,-5])
clusloglik(i.pam5.man$clustering, i.std.train[,-5], i.pam5.man.pred,
           i.std.test[,-5])
clusloglik(i.pam7.man$clustering, i.std.train[,-5], i.pam7.man.pred,
           i.std.test[,-5])
