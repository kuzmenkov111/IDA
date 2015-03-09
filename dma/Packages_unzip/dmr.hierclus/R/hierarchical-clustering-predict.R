## hierarchical clustering prediction
predict.hcl <- function(model, data, ...)
{
  if (!is.null(model$data) && !is.null(model$link))
    predict.ahc(model, data, ...)
  else if (!is.null(model$centers))
    predict.dhc(model, data, ...)
}

## agglomerative hierarchical clustering prediction
predict.ahc <- function(model, data, diss=euc.dist)
{
  ext.data <- rbind(model$data, data)
  dm <- as.matrix(dissmat(ext.data, diss))  # dissimilarity matrix for linkage

  clusters <- sort(unique(model$clustering))
  x.clusters <- length(clusters) + 1:nrow(data)
  ext.clustering <- c(model$clustering, x.clusters)

  links <- outer(clusters, x.clusters,
                 Vectorize(function(d1, d2)
                           model$link(ext.clustering, d1, d2, ext.data, diss, dm)))
  apply(links, 2, which.min)
}

## divisive hierarchical clustering prediction
predict.dhc <- function(model, data, diss=euc.dist)
{
  centers <- do.call(rbind,
                     lapply(1:nrow(model$merge),
                            function(i)
                            model$centers[[i]][model$merge[i,]<0,]))
  clusters <- -t(model$merge)[t(model$merge)<0]
  centers <- centers[match(1:length(clusters), clusters),]  # reorder centers
  k.centers.assign(centers, data, diss)
}


if (FALSE)
{

  # hierarchical clustering prediction for the weathercl data
predict(wcl.ahc.cl, wcl.std)
predict(wcl.dhc, wcl.std)

  # hierarchical clustering prediction for the iris data
i.ahc.cl.cd3.pred <- predict(i.ahc.cl.cd3, i.std.test[,-5])
i.ahc.sl.cd3.pred <- predict(i.ahc.sl.cd3, i.std.test[,-5])
i.ahc.al.cd3.pred <- predict(i.ahc.al.cd3, i.std.test[,-5])
i.ahc.ml.cd3.pred <- predict(i.ahc.ml.cd3, i.std.test[,-5])
i.ahc.wl.cd3.pred <- predict(i.ahc.wl.cd3, i.std.test[,-5])
i.dhc.cd3.pred <- predict(i.dhc.cd3, i.std.test[,-5])

  # hierarchical clustering prediction for the Glass data
g.ahc.cl.cd3.pred <- predict(g.ahc.cl.cd3, g.std.test[,-10])
g.ahc.sl.cd3.pred <- predict(g.ahc.sl.cd3, g.std.test[,-10])
g.ahc.al.cd3.pred <- predict(g.ahc.al.cd3, g.std.test[,-10])
g.ahc.ml.cd3.pred <- predict(g.ahc.ml.cd3, g.std.test[,-10])
g.ahc.wl.cd3.pred <- predict(g.ahc.wl.cd3, g.std.test[,-10])
g.dhc.cd3.pred <- predict(g.dhc.cd3, g.std.test[,-10])

}
