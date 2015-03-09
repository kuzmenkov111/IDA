## subset evaluation for correlation-based filters
cfs.eval <- function(subset, target, data, cormat)
{
  if (length(subset <- unique(subset))>0)
  {
      cor.at <- mean(sapply(subset, function(a) cormat[a,target]))
      cor.aa <- mean(outer(subset, subset,
                           Vectorize(function(a1, a2)
                                     ifelse(a1!=a2, cormat[a1,a2], NA))),
                     na.rm=TRUE)
      cor.aa <- ifelse(is.finite(cor.aa), cor.aa, 0)
      length(subset)*cor.at/
        sqrt(length(subset)+length(subset)*(length(subset)-1)*cor.aa)
  }
  else
    -Inf
}

## correlation-based filter
cfs.filter <- function(formula, data, corf=symunc,
                       searchf=asel.search.greedy,
                       initf=asel.init.all, nextf=asel.next.backward)
{
  target <- y.var(formula)
  attributes <- x.vars(formula, data)
  atnames <- c(attributes, target)

  cormat <- outer(1:length(atnames), 1:length(atnames),
                  Vectorize(function(i, j)
                            ifelse(j<i, corf(data[[atnames[i]]], data[[atnames[j]]]),
                                   NA)))
  cormat[upper.tri(cormat)] <- t(cormat)[t(lower.tri(cormat))]
  dimnames(cormat) <- list(atnames, atnames)

  searchf(attributes, target,
          evalf=function(subset, target) cfs.eval(subset, target, data, cormat),
          initf=initf, nextf=nextf)
}

  # correlation-based filter for the weather data
cfs.filter(play~., weather)
cfs.filter(play~., weather, initf=asel.init.none, nextf=asel.next.forward)
cfs.filter(play~., weather, corf=discor)
cfs.filter(play~., weather, corf=discor,
           initf=asel.init.none, nextf=asel.next.forward)

  # correlation-based filter for the weatherc data
cfs.filter(play~., discnm.eqfreq(~., weatherc, 4))
cfs.filter(play~., weatherc, corf=discor)
cfs.filter(play~., weatherc, corf=discor,
           initf=asel.init.none, nextf=asel.next.forward)

  # correlation-based filter for the weatherr data
cfs.filter(playability~., weatherr, corf=discor)
cfs.filter(playability~., weatherr, corf=discor,
           initf=asel.init.none, nextf=asel.next.forward)

  # correlation-based filter for the Vehicle Silhouettes data
v.sel.cfs <- cfs.filter(Class~., discnm.eqfreq(~., v.train, 7))$subset
  # correlation-based filter for the Soybean data
s.sel.cfs <- cfs.filter(Class~., s.train)$subset
  # correlation-based filter for the Boston Housing data
bh.sel.cfs <- cfs.filter(medv~., discnm.eqfreq(~., bh.train, 7))$subset
