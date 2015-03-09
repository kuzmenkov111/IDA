## RELIEF dissimilarity
relief.diss <- function(x1, x2, rngs)
{
  ifelse(is.na(rd <- mapply(function(v1, v2, r)
                            ifelse(is.numeric(v1), abs(v1-v2)/r, v1!=v2),
                            x1, x2, rngs)),
         0, rd)
}

## RELIEF filter
relief.filter <- function(formula, data, k=1, K=floor(0.1*nrow(data)))
{
  attributes <- x.vars(formula, data)
  class <- y.var(formula)
  aind <- names(data) %in% attributes
  rngs <- ranges(data)[aind]

  util <- sapply(attributes, function(a) 0)

  for (i in 1:K)
  {
    xi <- sample(nrow(data), 1)
    x <- data[xi, ]
    data.x <- data[-xi,]
    hits <- arg.min((1:nrow(data.x))[data.x[[class]]==x[[class]]],
                    function(j) sum(relief.diss(data.x[j,aind], x[aind], rngs)),
                    k=k)
    misses <- arg.min((1:nrow(data.x))[data.x[[class]]!=x[[class]]],
                      function(j) sum(relief.diss(data.x[j,aind], x[aind], rngs)),
                      k=k)
    util <- util +
      rowSums(sapply(misses,
                     function(j) relief.diss(data.x[j,aind], x[aind], rngs)))/K-
        rowSums(sapply(hits,
                       function(j) relief.diss(data.x[j,aind], x[aind], rngs)))/K
  }
  sort(util, decreasing=TRUE)
}

  # RELIEF for the weather data
relief.filter(play~., weather, K=100)
relief.filter(play~., weather, k=3, K=100)

  # RELIEF for the weatherc data
relief.filter(play~., weatherc, K=100)
relief.filter(play~., weatherc, k=3, K=100)

  # RELIEF for the Vehicle Silhouettes data
v.utl.rel <- relief.filter(Class~., v.train, k=3, K=200)
  # RELIEF for the Soybean data
s.utl.rel <- relief.filter(Class~., s.train, k=3, K=200)
