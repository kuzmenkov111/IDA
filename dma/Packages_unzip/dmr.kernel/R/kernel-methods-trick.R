## data transformation that generates new attributes
## defined as the products of all original attribute pairs
trans.mult2 <- function(data)
{
  t(apply(data, 1, function(d) d %o% d))
}


if (FALSE)
{

  # original dataset
kmdat.orig <- kmdat.train[1:10,1:4]
  # dot product matrix for the original dataset
kmdat.dp <- as.matrix(kmdat.orig) %*% t(kmdat.orig)

  # transformed dataset
kmdat.trans <- trans.mult2(kmdat.orig)
  # dot product matrix for the transformed dataset
kmdat.dpt <- kmdat.trans %*% t(kmdat.trans)

  # verify that the dot product matrix for the transformed dataset
  # is the same as the squared original dot product matrix
max(abs((kmdat.dpt-kmdat.dp^2)))

}
