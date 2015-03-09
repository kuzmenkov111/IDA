## divisive clustering using alg, which is assumed to be called:
##   alg(data, 2, ...)
dhc <- function(data, alg=pam, cls="clustering", cnt="medoids", centf=as.numeric,
                maxdepth=16, ...)
{
  clustering <- rep(1, nrow(data))  # cluster membership assignment
  centers <- NULL  # cluster centers
  merge <- NULL
  height <- NULL

  while (any(clustering>0))
  {
    d <- min(clustering[clustering>0])  # cluster to process
    if ((m <- sum(clustering==d))>1 && d<2^maxdepth)
    {

      cls.d <- if (m>2) (mod.d <- alg(data[clustering==d,], 2, ...))[[cls]] else 1:2
      centers <- c(list(if (m>2) mod.d[[cnt]]
                        else sapply(data[clustering==d,], centf)), centers)
      clustering[clustering==d] <- 2*d + (cls.d-1)
      merge <- rbind(c(2*d, 2*d+1), merge)
      height <- c(height, length(height)+1)
    }
    else
    {
      clustering[clustering==d] <- -d   # mark as leaf
      merge[merge==d] <- -d
    }
  }

  bottom <- unique(clustering)
  clustering <- (1:length(bottom))[match(clustering, bottom)]  # re-assign ids
  merge[merge<0] <- -(1:length(bottom))[match(merge[merge<0], bottom)]
  merge[merge>0][order(merge[merge>0])] <- sum(merge>0):1
  `class<-`(list(clustering=clustering, centers=centers, merge=merge, height=height,
                 order=-t(merge)[t(merge)<0]),
            "hcl")
}


if (FALSE)
{

  # divisive clustering for the weathercl data
wcl.dhc <- dhc(wcl.std)

  # divisive hierarchical clustering for the iris data
i.dhc <- dhc(i.std.train[,-5])
i.dhc.km <- dhc(i.std.train[,-5], alg=kmeans, cls="cluster", cnt="centers")
i.dhc.d3 <- dhc(i.std.train[,-5], maxdepth=3)
i.dhc.km.d3 <- dhc(i.std.train[,-5], alg=kmeans, cls="cluster", cnt="centers",
                   maxdepth=3)

  # divisive hierarchical clustering for the Glass data
g.dhc <- dhc(g.std.train[,-10]  )
g.dhc.km <- dhc(g.std.train[,-10], alg=kmeans, cls="cluster", cnt="centers")
g.dhc.d3 <- dhc(g.std.train[,-10], maxdepth=3)
g.dhc.km.d3 <- dhc(g.std.train[,-10], alg=kmeans, cls="cluster", cnt="centers",
                   maxdepth=3)

}
