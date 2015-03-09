## regression RELIEF filter
rrelief.filter <- function(formula, data, k=1, K=floor(0.1*nrow(data)), sigma=10)
{
  attributes <- x.vars(formula, data)
  target <- y.var(formula)
  aind <- names(data) %in% attributes
  rngs <- ranges(data)[aind]
  data <- predict.nrm(nrm.all(make.formula("", target), data), data)

  delta.f <- 0
  delta.i <- delta.fi <- sapply(attributes, function(a) 0)

  for (i in 1:K)
  {
    xi <- sample(nrow(data), 1)
    x <- data[xi, ]
    data.x <- data[-xi,]
    diss <- sapply(1:nrow(data.x),
                   function(j) sum(relief.diss(data.x[j,aind], x[aind], rngs)))
    neighbors <- arg.min(1:nrow(data.x), function(j) diss[j], k=k)
    nbranks <- rank(diss[neighbors])
    weights <- (weights <- exp(-(nbranks/sigma)^2))/sum(weights)
    adiff <- sapply(neighbors, function(j)
                               relief.diss(data.x[j,aind], x[aind], rngs))
    fdiff <- abs(data.x[neighbors,target]-x[,target])
    delta.f <- delta.f + sum(fdiff*weights)
    delta.i <- delta.i + rowSums(adiff %*% diag(weights, nrow=length(weights)))
    delta.fi <- delta.fi +
                  rowSums(adiff %*% diag(fdiff*weights, nrow=length(weights)))
  }
  sort(util<-delta.fi/delta.f-(delta.i-delta.fi)/(K-delta.f), decreasing=TRUE)
}

  # RELIEF for the weatherr data
rrelief.filter(playability~., weatherr, K=100)
rrelief.filter(playability~., weatherr, k=3, K=100)

  # RELIEF for the Boston Housing data
bh.utl.rel <- rrelief.filter(medv~., bh.train, k=3, K=200)
