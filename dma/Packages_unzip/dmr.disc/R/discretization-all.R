## create a wrapper for all attributes discretization
disc.all <- function(disc)
{
  disc1 <- function(v, k, class, ...)
  {
    if (is.numeric(v))
    {
      if (is.null(formals(disc)$class))
        disc(v, k=k, ...)  # unsupervised
      else
        disc(v, k=k, class=class, ...)  # supervised
    }
  }

  function(formula, data, k=5, ...)
  {
    attributes <- x.vars(formula, data)
    class <- y.var(formula)
    if (length(k)==1)
      k <- sapply(attributes, function(a) k)
    km <- match(attributes, names(k))
    `class<-`(mapply(function(a, k1) disc1(data[[a]], k=k1, class=data[[class]], ...),
                     attributes, k[km], SIMPLIFY=FALSE),
              "disc")
  }
}

## apply discretization breaks to a dataset
predict.disc <- predict.transmod(function(m, v) cut(v, c(-Inf, m, Inf)))


if (FALSE)
{

  # random all-attributes discretization
disc.rand <- disc.all(function(v, k=3) sort(runif(k-1, min=min(v), max=max(v))))

  # random discretization for the weatherc data
w.dr3m <- disc.rand(play~., weatherc, 3)
w.dr43m <- disc.rand(play~., weatherc, list(temperature=4, humidity=3))

  # apply discretization breaks to the weatherc data
w.dr3 <- predict(w.dr3m, weatherc)
w.dr43 <- predict(w.dr43m, weatherc)

}
