## average linkage
ahc.average <- function(clustering, d1, d2, data, diss, dm)
{ mean(dm[clustering==d1, clustering==d2]) }


if (FALSE)
{

  # agglomerative hierarchical average-linkage clustering for the weathercl data
wcl.ahc.al <- ahc(wcl.std, linkf=ahc.average)

  # agglomerative hierarchical average-linkage clustering for the iris data
i.ahc.al <- ahc(i.std.train[,-5], linkf=ahc.average)

  # agglomerative hierarchical average-linkage clustering for the Glass data
g.ahc.al <- ahc(g.std.train[,-10], linkf=ahc.average)

}
