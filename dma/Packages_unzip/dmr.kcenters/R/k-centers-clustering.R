k.centers <- function(data, k, diss=euc.dist, max.iter=10,
                      init=k.centers.init.rand,
                      assign=k.centers.assign,
                      adjust=k.centers.adjust.dummy)
{
  dm <- dissmat(data, diss)
  centers <- init(data, k)
  clustering <- NULL
  iter <- 0
  repeat
  {
    iter <- iter+1
    clustering.old <- clustering
    clustering <- assign(centers, data, diss, dm)
    centers <- adjust(clustering, data, k, diss, dm)
    if (iter >= max.iter || all(clustering==clustering.old))
      break
  }

  `class<-`(list(centers=centers, clustering=clustering), "k.centers")
}
