transnonmod.all <- function(transnm, condf=function(v) TRUE)
{
  function(formula, data, ...)
  {
    attributes <- x.vars(formula, data)
    as.data.frame(sapply(names(data),
                         function(a)
                           if (a %in% attributes && condf(data[[a]]))
                             transnm(data[[a]], ...)
                           else data[[a]],
                         simplify=FALSE))
  }
}

  # simple centering (mean subtraction) transformation
center.nm <- transnonmod.all(function(v) v-mean(v), is.numeric)
  # performed on the weatherc data
center.nm(play~., weatherc)

  # simple round to a multiple of 10 transformation
divmod10 <- transnonmod.all(function(v) cbind(v %/% 10, v %% 10), is.numeric)
  # performed on the weatherc data
divmod10(play~., weatherc)
