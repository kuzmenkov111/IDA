## m-estimate of probability of an event occurring n1 out of n times
## incorporating m fictitious instances
mest <- function(n1, n, m=2, p0=1/m) { (n1+m*p0)/(n+m) }

mprob <- function(v, v1, m=nlevels(v), p0=1/nlevels(v))
{ mest(sum(v==v1), length(v), m, p0) }

  # demonstration
mest(0, 10, 1, 0.5)
mest(0, 10, 2, 0.5)
mest(10, 10, 1, 0.5)
mest(10, 10, 2, 0.5)

mprob(weather$outlook, "rainy", m=0)
mprob(weather$outlook, "rainy")
mprob(weather$play[weather$outlook=="overcast"], "no", m=0)
mprob(weather$play[weather$outlook=="overcast"], "no")
mprob(weather$play[weather$outlook=="overcast"], "no", m=3, p0=0.5)
