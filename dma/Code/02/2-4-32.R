## Laplace estimate of probability of an event occurring n1 out of n times
## with m possible outcomes
laest <- function(n1, n, m=2) { mest(n1, n, m)  }

laprob <- function(v, v1) { mprob(v, v1) }

  # demonstration
laest(0, 10, 2)
mest(0, 10, 2)
laest(10, 10, 2)
mest(10, 10, 2)

laprob(weather$outlook, "rainy")
mprob(weather$outlook, "rainy", m=3, p0=1/3)
laprob(weather$play[weather$outlook=="overcast"], "no")
mprob(weather$play[weather$outlook=="overcast"], "no", m=2, p0=0.5)
