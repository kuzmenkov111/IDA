## equal-width discretization for a single attribute
disc.eqwidth1 <- function(v, k=5)
{
  w <- diff(r <- range(v))/k
  seq(r[1]+w, r[2]-w, w)
}

## equal-width discretization for a dataset
disc.eqwidth <- disc.all(disc.eqwidth1)


if (FALSE)
{

  # equal-width discretization of the temperature attribute in the weatherc data
disc.eqwidth1(weatherc$temperature, 4)

  # equal-width discretization for the weatherc data
disc.eqwidth(play~., weatherc, 3)
disc.eqwidth(play~., weatherc, list(temperature=4, humidity=3))

  # equal-width discretization for the Vehicle Silhouette data
v.disc.ew <- disc.eqwidth(Class~., v.train, 7)
summary(predict(v.disc.ew, v.train))

  # equal-width discretization for the Glass data
g.disc.ew <- disc.eqwidth(Type~., g.train, 7)
summary(predict(g.disc.ew, g.train))

}
