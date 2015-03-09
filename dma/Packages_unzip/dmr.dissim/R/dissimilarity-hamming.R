ham.dist <- function(x1, x2) { sum(x1!=x2, na.rm=TRUE) }


if (FALSE)
{

  # Hamming distance dissimilarity matrix for the weathercl
dissmat(weathercl, ham.dist)

}
