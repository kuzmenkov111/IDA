## single linkage
ahc.single <- function(clustering, d1, d2, data, diss, dm)
{ min(dm[clustering==d1, clustering==d2]) }

  # agglomerative hierarchical single-linkage clustering for the weathercl data
wcl.ahc.sl <- ahc(wcl.std, linkf=ahc.single)

  # agglomerative hierarchical single-linkage clustering for the iris data
i.ahc.sl <- ahc(i.std.train[,-5], linkf=ahc.single)

  # agglomerative hierarchical single-linkage clustering for the Glass data
g.ahc.sl <- ahc(g.std.train[,-10], linkf=ahc.single)
