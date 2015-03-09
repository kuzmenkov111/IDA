## wrap single-attribute modeling transformation transm
## so that it is applied to all attributes for which condf returns TRUE
transmod.all <- function(transm, condf=function(v) TRUE)
{
  function(formula, data, ...)
  {
    attributes <- x.vars(formula, data)
    sapply(attributes,
           function(a) if (condf(data[[a]])) transm(data[[a]], ...),
           simplify=FALSE)
  }
}

## apply transformation model to a dataset
predict.transmod <- function(pred.transm)
{
  function(model, data, ...)
  {
    as.data.frame(sapply(names(data),
                         function(a)
                         if (a %in% names(model) && !is.null(model[[a]]))
                           pred.transm(model[[a]], data[[a]], ...)
                         else data[[a]],
                         simplify=FALSE))
  }
}


if (FALSE)
{

  # simple centering (mean subtraction) transformation
center.m <- transmod.all(mean, is.numeric)
  # performed on the weatherc data
w.cm <- center.m(play~., weatherc)
  # applied to the weatherc data
predict.center.m <- predict.transmod(function(m, v) v-m)
predict.center.m(w.cm, weatherc)

}
