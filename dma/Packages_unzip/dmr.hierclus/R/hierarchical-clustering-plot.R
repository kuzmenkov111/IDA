## convert to dendrogram
as.dendrogram.hcl <- function(model) { as.dendrogram(as.hclust(model)) }

## plot a hierarchical clustering dendrogram
plot.hcl <- function(model, ...)
{
  plot(as.dendrogram(model), center=TRUE, ...)
}


if (FALSE)
{

  # dendrogram plots for the weathercl data
par(mfrow=c(3, 2))
plot(wcl.ahc.sl, main="Single linkage")
plot(wcl.ahc.cl, main="Complete linkage")
plot(wcl.ahc.al, main="Average linkage")
plot(wcl.ahc.ml, main="Center linkage")
plot(wcl.ahc.wl, main="Ward linkage")
plot(wcl.dhc, main="Divisive clustering")

}
