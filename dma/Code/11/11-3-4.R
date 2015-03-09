ravdiff <- function(x1, x2)
{
  mapply(function(v1, v2) ifelse(is.numeric(v1), (v1-v2)/(abs(v1)+abs(v2)), v1!=v2),
         x1, x2)
}

can.dist <- function(x1, x2) { sum(abs(ravdiff(x1,x2)), na.rm=TRUE) }

  # Canberra distance dissimilarity matrix for the weathercl data
dissmat(weathercl, function (x1, x2) can.dist(x1, x2))
