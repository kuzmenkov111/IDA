## decision tree prediction
predict.dectree <- function(tree, data)
{
  descend <- function(n)
  {
    if (!is.na(tree$attribute[tree$node==n]))  # unless reached a leaf
    {
      av <- data[[tree$attribute[tree$node==n]]]
      cond <- !is.na(av) & (if (is.numeric(av))
                              av<=tree$value[tree$node==n]
                            else
                              av==tree$value[tree$node==n])

      nodemap[nodemap==n & cond] <<- 2*n
      nodemap[nodemap==n & !cond] <<- 2*n+1
      descend(2*n)
      descend(2*n+1)
    }
  }

  nodemap <- rep(1, nrow(data))
  descend(1)
  tree$class[match(nodemap, tree$node)]
}

   # decision tree prediction for the weather data
predict(tree, weather)
  # decision tree prediction for the weatherc data
predict(treec, weatherc)
