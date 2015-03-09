## cut a hierarchical clustering model to k clusters
cut.hcl <- function(model, k)
{
  nc <- maxc <- nrow(model$merge)+1  # number of clusters
  k <- clip.val(k, 2, nc)            # make sure k is in the valid range

  clustering <- model$clustering
  merge <- model$merge
  height <- model$height
  while (nc>k)
  {
    mr <- merge[1,]    # merge to remove
    merge <- merge[-1,]
    clustering[clustering %in% -mr] <- (maxc <- maxc + 1)  # id for new leaf
    merge[merge>0] <- merge[merge>0]-1  # shift node numbers
    merge[merge==0] <- -maxc
    height <- height[-1]-(height[2]-height[1])
    nc <- nc-1
  }

  bottom <- unique(clustering)
  clustering <- (1:length(bottom))[match(clustering, bottom)]  # re-assign ids
  merge[merge<0] <- -(1:length(bottom))[match(merge[merge<0], -bottom)]

  model$clustering <- clustering
  model$centers <- model$centers[-(1:(length(model$centers)-k+1))]
  model$merge <- merge
  model$height <- height
  model$order <- -t(merge)[t(merge)<0]
  model
}

  # cutting hierarchical clustering trees for the weathercl data
wcl.ahc.sl.c4 <- cut(wcl.ahc.sl, 4)
wcl.ahc.cl.c4 <- cut(wcl.ahc.cl, 4)
wcl.dhc.c4 <- cut(wcl.dhc, 4)

  # cutting hierarchical clustering trees for the iris data
i.ahc.sl.cd3 <- cut(i.ahc.sl, max(i.dhc.d3$clustering))
i.ahc.cl.cd3 <- cut(i.ahc.cl, max(i.dhc.d3$clustering))
i.ahc.al.cd3 <- cut(i.ahc.al, max(i.dhc.d3$clustering))
i.ahc.ml.cd3 <- cut(i.ahc.ml, max(i.dhc.d3$clustering))
i.ahc.wl.cd3 <- cut(i.ahc.wl, max(i.dhc.d3$clustering))
i.dhc.cd3 <- cut(i.dhc, max(i.dhc.d3$clustering))
  # verify i.dhc.cd3 and i.dhc.d3 are the same
all(i.dhc.cd3$clustering==i.dhc.d3$clustering)
all(i.dhc.cd3$merge==i.dhc.d3$merge)
all(sapply(1:length(i.dhc.cd3$centers),
           function(d) all(i.dhc.cd3$centers[[d]]==i.dhc.d3$centers[[d]])))

  # cutting hierarchical clustering trees for the Glass data
g.ahc.sl.cd3 <- cut(g.ahc.sl, max(g.dhc.d3$clustering))
g.ahc.cl.cd3 <- cut(g.ahc.cl, max(g.dhc.d3$clustering))
g.ahc.al.cd3 <- cut(g.ahc.al, max(g.dhc.d3$clustering))
g.ahc.ml.cd3 <- cut(g.ahc.ml, max(g.dhc.d3$clustering))
g.ahc.wl.cd3 <- cut(g.ahc.wl, max(g.dhc.d3$clustering))
g.dhc.cd3 <- cut(g.dhc, max(g.dhc.d3$clustering))
  # verify g.dhc.cd3 and g.dhc.d3 are the same
all(g.dhc.cd3$clustering==g.dhc.d3$clustering)
all(g.dhc.cd3$merge==g.dhc.d3$merge)
all(sapply(1:length(g.dhc.cd3$centers),
           function(d) all(g.dhc.cd3$centers[[d]]==g.dhc.d3$centers[[d]])))
