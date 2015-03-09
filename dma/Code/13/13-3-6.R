## Ward linkage
ahc.ward <- function(clustering, d1, d2, data, diss, dm)
{
  c1 <- attr.mm(data[clustering==d1,])
  c2 <- attr.mm(data[clustering==d2,])
  c12 <- attr.mm(data[clustering==d1 | clustering==d2,])

  sum(sapply(which(clustering==d1 | clustering==d2),
             function(i) diss(data[i,], c12)^2)) -
    sum(sapply(which(clustering==d1), function(i) diss(data[i,], c1)^2)) -
    sum(sapply(which(clustering==d2), function(i) diss(data[i,], c2)^2))
}

  # agglomerative hierarchical Ward-linkage clustering for the weathercl data
wcl.ahc.wl <- ahc(wcl.std, linkf=ahc.ward)

  # agglomerative hierarchical Ward-linkage clustering for the iris data
i.ahc.wl <- ahc(i.std.train[,-5], linkf=ahc.ward)

  # agglomerative hierarchical Ward-linkage clustering for the Glass data
g.ahc.wl <- ahc(g.std.train[,-10], linkf=ahc.ward)
