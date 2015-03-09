avdiff <- function(x1, x2)
{
  mapply(function(v1, v2) ifelse(is.numeric(v1), v1-v2, v1!=v2), x1, x2)
}

euc.dist <- function(x1, x2) { sqrt(sum(avdiff(x1,x2)^2, na.rm=TRUE)) }

  # Euclidean distance dissimilarity matrix for the weathercl data
dissmat(weathercl, euc.dist)
