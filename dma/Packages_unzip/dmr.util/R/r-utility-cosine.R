## cosine of the angle between two continuous vectors
cosine <- function(v1, v2) { sum(v1*v2, na.rm=TRUE)/(l2norm(v1)*l2norm(v2)) }

if (FALSE)
{

  # usage examples
cosine(1:5, 1:5)
cosine(1:5, 5:1)
cosine(1:5, -1:-5)
cosine(1:5, rep(3,5))

}
