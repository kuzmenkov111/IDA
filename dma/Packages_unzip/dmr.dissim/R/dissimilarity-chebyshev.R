cheb.dist <- function(x1, x2) { max(abs(avdiff(x1,x2)), na.rm=TRUE) }


if (FALSE)
{

  # Chebyshev distance dissimilarity matrix for the weathercl data
dissmat(weathercl, cheb.dist)
  # roughly the same as
dissmat(weathercl, function (x1, x2) mink.dist(x1, x2, 10))

}
