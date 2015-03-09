## binary encoding of a single discrete attribute value
discode1 <- function(v, b=c(0,1), red=FALSE, na.all=FALSE)
{
  r <- 1-as.integer(red)
  if (is.factor(v) && ! (is.na(v) && na.all))
    b[1+as.integer(v==levels(v)[1:(nlevels(v)-r)])]
  else if (is.factor(v))
    rep(b[2], nlevels(v)-r)
  else
    v
}

## binary encoding of an discrete attribute
discode.a <- function(a, b=c(0,1), red=FALSE, na.all=FALSE)
{
  do.call(rbind, lapply(a, discode1, b, red, na.all))
}

## binary encoding of all discrete attributes in a given dataset
discode <- transnonmod.all(discode.a)


if (FALSE)
{

  # encoding a single attribute value
discode1(weatherc$outlook[1])
discode1(weatherc$outlook[1], red=TRUE)
discode1(factor(NA, levels=levels(weatherc$outlook)))
discode1(factor(NA, levels=levels(weatherc$outlook)), na.all=TRUE)

  # encoding single attributes of the weatherc data
discode.a(weatherc$outlook)
discode.a(weatherc$temperature)
discode.a(weatherc$outlook, b=c(-1,1), red=TRUE)
discode.a(weatherc$wind, b=c(-1,1))

  # encoding single instances of the weatherc data
discode(~., weatherc[1,])
discode(~., weatherc[1,], red=TRUE)

  # encoding the complete weatherc data
discode(~., weatherc)
  # leave the target attribute unchanged
discode(play~., weatherc)

}
