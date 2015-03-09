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

k.centers.init.rand <- function(data, k) { data[sample(1:nrow(data), k),] }

k.centers.assign <- function(centers, data, diss, dm)
{
  center.diss <- function(i)
  { sapply(1:nrow(centers), function(d) diss(data[i,], centers[d,])) }
  assign1 <- function(i) { which.min(center.diss(i)) }
  sapply(1:nrow(data), assign1)
}

predict.k.centers <- function(model, data, diss=euc.dist)
{
  k.centers.assign(model$centers, data, diss)
}

k.centers.adjust.dummy <- function(clustering, data, k, diss, dm)
{
  do.call(rbind, lapply(1:k, function(d) data[which.max(clustering==d),]))
}

  # dummy k-centers clustering for the weathercl data
k.centers(wcl.std, 3)
