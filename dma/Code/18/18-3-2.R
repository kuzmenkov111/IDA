## equal-frequency discretization for a single attribute
disc.eqfreq1 <- function(v, k=5) { unique(quantile(v, seq(1/k, 1-1/k, 1/k))) }

## equal-frequency discretization for a dataset
disc.eqfreq <- disc.all(disc.eqfreq1)

  # equal-width discretization of the temperature attribute in the weatherc data
disc.eqfreq1(weatherc$temperature, 4)

  # equal-frequency discretization for the weatherc data
disc.eqfreq(play~., weatherc, 3)
disc.eqfreq(play~., weatherc, list(temperature=4, humidity=3))

  # equal-frequency discretization for the Vehicle Silhouettes data
v.disc.ef <- disc.eqfreq(Class~., v.train, 7)
summary(predict(v.disc.ef, v.train))

  # equal-frequency discretization for the Glass data
g.disc.ef <- disc.eqfreq(Type~., g.train, 7)
summary(predict(g.disc.ef, g.train))
