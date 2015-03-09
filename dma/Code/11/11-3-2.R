mink.dist <- function(x1, x2, p) { (sum(abs(avdiff(x1,x2))^p, na.rm=TRUE))^(1/p) }

  # Minkowski distance dissimilarity matrices for the weathercl data
dissmat(weathercl, function (x1, x2) mink.dist(x1, x2, 1))
dissmat(weathercl, function (x1, x2) mink.dist(x1, x2, 3))
