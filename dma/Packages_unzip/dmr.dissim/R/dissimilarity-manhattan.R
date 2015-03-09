man.dist <- function(x1, x2) { mink.dist(x1, x2, 1) }


if (FALSE)
{

  # Manhattan distance dissimilarity matrix for the weathercl data
dissmat(weathercl, function (x1, x2) man.dist(x1, x2))

}
