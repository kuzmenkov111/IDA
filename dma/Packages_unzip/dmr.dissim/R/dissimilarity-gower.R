gower.coef <- function(x1, x2, rngs)
{
  mean(mapply(function(v1, v2, r)
              ifelse(is.numeric(v1), abs(v1-v2)/r, v1!=v2), x1, x2, rngs), na.rm=TRUE)
}


if (FALSE)
{

  # Gower's coefficient dissimilarity matrix for the weathercl data
dissmat(weathercl, function (x1, x2) gower.coef(x1, x2, ranges(weathercl)))

}
