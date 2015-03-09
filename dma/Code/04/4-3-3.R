## create a naive Bayes classifier
nbc <- function(formula, data)
{
  class <- y.var(formula)
  attributes <- x.vars(formula, data)

  cc <- integer(nlevels(data[[class]]))  # initialize class counts
  names(cc) <- levels(data[[class]])
  avc <- sapply(attributes,              # initialize attribute-value-class counts
                function(a)
                matrix(0, nrow=nlevels(data[[a]]), ncol=nlevels(data[[class]]),
                       dimnames=list(levels(data[[a]]), levels(data[[class]]))))

  for (i in (1:nrow(data)))              # iterate through training instances
  {
    cc[data[[class]][i]] <- cc[data[[class]][i]]+1  # increment class count
    for (a in attributes)                # increment attribute-value-class counts
      avc[[a]][data[[a]][i],data[[class]][i]] <-
        avc[[a]][data[[a]][i],data[[class]][i]]+1
  }

    # calculate probability estimates based on counts
  `class<-`(list(prior=cc/sum(cc),
                 cond=sapply(avc, function(avc1)
                                  t(apply(avc1, 1, "/", colSums(avc1))))),
            "nbc")
}

  # naive Bayes classifier for the weather data
nbw <- nbc(play~., weather)
