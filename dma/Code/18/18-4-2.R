## bottom-up discretization for a single attribute
disc.bottomup1 <- function(v, class, k=5, initf=disc.pure1, evalf, maxev=Inf)
{
  breaks <- initf(v, class)
  utils <- mapply(evalf, breaks,
                  shift.right(breaks, first=-Inf), shift.left(breaks, last=Inf),
                  MoreArgs=list(v, class))
  b <- which.min(utils)

  while (length(breaks)+1>k && utils[b]<=maxev)
  {
    breaks <- breaks[-b]
    utils <- utils[-b]
    if (b>1)
      utils[b-1] <- evalf(breaks[b-1],
                          ifelse(b>2, breaks[b-2], -Inf),
                          ifelse(b<=length(breaks), breaks[b], Inf),
                          v, class)
    if (b<=length(breaks))
      utils[b] <- evalf(breaks[b],
                        ifelse(b>1, breaks[b-1], -Inf),
                        ifelse(b<length(breaks), breaks[b+1], Inf),
                        v, class)

    b <- which.min(utils)
  }
  breaks
}

## bottom-up discretization for a dataset
disc.bottomup <- disc.all(disc.bottomup1)

  # bottom-up discretization of the temperature attribute in the weatherc data
disc.bottomup1(weatherc$temperature, weatherc$play, 3,
               evalf=function(b, bl, br, v, class) b)

  # bottom-up discretization for the weatherc data
disc.bottomup(play~., weatherc, 3, evalf=function(b, bl, br, v, class) b)
