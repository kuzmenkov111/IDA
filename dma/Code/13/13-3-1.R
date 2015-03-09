## agglomerative hierarchical clustering
ahc <- function(data, linkf=ahc.size, diss=euc.dist, bottom=1:nrow(data))
{
    # hclust-compatible cluster id scheme
  clid <- function(d)
  { if (d>length(bottom.clusters)) d-length(bottom.clusters) else -d }

  dm <- as.matrix(dissmat(data, diss))  # instance dissimilarity matrix for linkage

  bottom.clusters <- unique(bottom)  # bottom-level clusters
  clustering <- bottom               # current cluster assignment
  clusters <- bottom.clusters        # current set of clusters

  merge <- NULL   # merge matrix
  height <- NULL  # height vector
  order <- NULL   # order vector

  links <- outer(1:length(clusters), 1:length(clusters),
                 Vectorize(function(i1, i2)
                           if (i1<i2)
                             linkf(clustering, clusters[i1], clusters[i2], data,
                                   diss, dm)
                           else NA))
  while(length(clusters)>1)
  {
    mli <- arrayInd(which.min(links), dim(links))  # minimum link index
    d1 <- clusters[i1 <- mli[1]]
    d2 <- clusters[i2 <- mli[2]]
    d12 <- max(clusters)+1
        # merge d1 and d2 into d12
    merge <- rbind(merge, c(clid(d1), clid(d2)))
    height <- c(height, if (is.null(height) || links[i1,i2]>height[length(height)])
                          links[i1,i2]
                        else height[length(height)]+height[1])  # height correction
    clustering[clustering==d1 | clustering==d2] <- d12
    clusters <- clusters[-c(i1, i2)]
    links <- links[-c(i1, i2),,drop=FALSE]  # remove links for d1
    links <- links[,-c(i1, i2),drop=FALSE]  # remove links for d2
    if (length(clusters)>0)
    {
      links <- cbind(links, sapply(clusters,
                                   function(d) linkf(clustering, d, d12, data,
                                                     diss, dm)))
      links <- rbind(links, NA)  # keep the matrix square
    }
    clusters <- c(clusters, d12)
  }

  `class<-`(list(clustering=bottom, link=linkf, data=data,
                 merge=merge, height=height, order=-t(merge)[t(merge)<0]),
            "hcl")
}

## convert to hclust
as.hclust.hcl <- function(model) { `class<-`(unclass(model), c("hclust")) }

## size linkage (dummy)
ahc.size <- function(clustering, d1, d2, data, diss, dm)
{ sum(clustering==d1) + sum(clustering==d2) }

  # agglomerative clustering for the weathercl data
wcl.ahc.d <- ahc(wcl.std, linkf=ahc.size)
as.hclust(wcl.ahc.d)
