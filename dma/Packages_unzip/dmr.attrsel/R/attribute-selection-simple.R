dd.chi2 <- function(a1, a2) 1-chisq.test(a1, a2)$p.value
cd.kruskal <- function(a1, a2) 1-kruskal.test(a1, a2)$p.value
cc.spearman <- function(a1, a2) 1-cor.test(a1, a2, method="spearman")$p.value

## simple statistical attribute selection filter
simple.filter <- function(formula, data, dd=dd.chi2, cd=cd.kruskal, cc=cc.spearman)
{
  attributes <- x.vars(formula, data)
  target <- y.var(formula)

  utility <- function(a)
  {
    unname(switch(attr.type(data[[a]], data[[target]]),
                  dd = dd(data[[a]], data[[target]]),
                  cd = cd(data[[a]], data[[target]]),
                  dc = cd(data[[target]], data[[a]]),
                  cc = cc(data[[a]], data[[target]])))
  }

  sort(sapply(attributes, utility), decreasing=TRUE)
}


if (FALSE)
{

  # simple filter for the weather data
simple.filter(play~., weather)
simple.filter(play~., weather, dd=symunc)

  # simple filter for the weatherc data
simple.filter(play~., weatherc)
simple.filter(play~outlook+wind, weatherc, dd=symunc)
simple.filter(play~temperature+humidity, weatherc,
              cd=function(a1, a2) kruskal.test(a1, a2)$statistic)

  # simple filter for the weatherr data
simple.filter(playability~., weatherr)
simple.filter(playability~outlook+wind, weatherr,
              cd=function(a1, a2) kruskal.test(a1, a2)$statistic)
simple.filter(playability~temperature+humidity, weatherr,
              cc=function(a1, a2) cor(a1, a2, method="spearman")^2)

  # simple filter for the Vehicle Silhouettes data
v.utl.simple <- simple.filter(Class~., discnm.eqfreq(~., v.train, 7), dd=symunc)
  # simple filter for the Soybean data
s.utl.simple <- simple.filter(Class~., Soybean, dd=symunc)
  # simple filter for the BostonHousing data
bh.utl.simple <- simple.filter(medv~., discnm.eqfreq(~., bh.train, 7), dd=symunc)

}
