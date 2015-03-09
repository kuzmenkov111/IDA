## center (mean/mode) linkage
ahc.center <- function(clustering, d1, d2, data, diss, dm)
{ diss(attr.mm(data[clustering==d1,]), attr.mm(data[clustering==d2,])) }


if (FALSE)
{

  # agglomerative hierarchical center-linkage clustering for the weathercl data
wcl.ahc.ml <- ahc(wcl.std, linkf=ahc.center)

  # agglomerative hierarchical center-linkage clustering for the iris data
i.ahc.ml <- ahc(i.std.train[,-5], linkf=ahc.center)

  # agglomerative hierarchical center-linkage clustering for the Glass data
g.ahc.ml <- ahc(g.std.train[,-10], linkf=ahc.center)

}
