## complete linkage
ahc.complete <- function(clustering, d1, d2, data, diss, dm)
{ max(dm[clustering==d1, clustering==d2]) }


if (FALSE)
{

  # agglomerative hierarchical complete-linkage clustering for the weathercl data
wcl.ahc.cl <- ahc(wcl.std, linkf=ahc.complete)

  # agglomerative hierarchical complete-linkage clustering for the iris data
i.ahc.cl <- ahc(i.std.train[,-5], linkf=ahc.complete)

  # agglomerative hierarchical complete-linkage clustering for the Glass data
g.ahc.cl <- ahc(g.std.train[,-10], linkf=ahc.complete)

}
