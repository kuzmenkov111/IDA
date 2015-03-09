## subset evaluation for consistency-based filters
cons.eval <- function(subset, target, data)
{
  if (require(digest, quietly=TRUE))
    hashfun <- function(x) digest(as.numeric(x))
  else
    hashfun <- function(x) paste(as.numeric(x), collapse="")
  aind <- names(data) %in% subset
  datahash <- sapply(1:nrow(data), function(j) hashfun(data[j,aind]))
  -sum(sapply(unique(datahash),
              function(xh)
              sum(datahash==xh)/nrow(data)*entropy(data[datahash==xh,target])))
}

## consistency-based filter
cons.filter <- function(formula, data,
                        searchf=asel.search.greedy,
                        initf=asel.init.all, nextf=asel.next.backward)
{
  target <- y.var(formula)
  attributes <- x.vars(formula, data)
  searchf(attributes, target,
          evalf=function(subset, target) cons.eval(subset, target, data),
          initf=initf, nextf=nextf)
}

  # consistency-based filter for the weather data
cons.filter(play~., weather)
cons.filter(play~., weather, initf=asel.init.none, nextf=asel.next.forward)

  # consistency-based filter for the weatherc data
cons.filter(play~., discnm.eqfreq(~., weatherc, 4))
cons.filter(play~., discnm.eqfreq(~., weatherc, 4),
            initf=asel.init.none, nextf=asel.next.forward)
cons.filter(play~., weatherc)
cons.filter(play~., weatherc, initf=asel.init.none, nextf=asel.next.forward)

  # correlation-based filter for the weatherr data
cons.filter(playability~., discnm.eqfreq(~., weatherr, 4))
cons.filter(playability~., discnm.eqfreq(~., weatherr, 4),
            initf=asel.init.none, nextf=asel.next.forward)

  # consistency-based for the Vehicle Silhouettes data
v.sel.cons <- cons.filter(Class~., discnm.eqfreq(~., v.train, 7))$subset
  # consistency-based for the Soybean data
s.sel.cons <- cons.filter(Class~., s.train)$subset
  # consistency-based for the Boston Housing data
bh.sel.cons <- cons.filter(medv~., discnm.eqfreq(~., bh.train, 7))$subset
