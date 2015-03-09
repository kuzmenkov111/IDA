## top-down discretization for a single attribute
disc.topdown1 <- function(v, class, k=5, candf=disc.pure1, evalf, minev=0)
{
  evalf.td <- function(b, bl, br, v, class)
  {
    if (any(v>bl & v<=br))
     evalf(b, bl, br, v, class)
    else
      -Inf
  }

  breaks <- NULL
  candidates <- candf(v, class)

  utils <- mapply(evalf.td, candidates,
                  closest.below(candidates, breaks),
                  closest.above(candidates, breaks),
                  MoreArgs=list(v, class))
  b <- which.max(utils)

  while (length(candidates)>0 && length(breaks)+1<k && utils[b]>=minev)
  {
    breaks <- insert.ord(breaks, candidates[b])
    candidates <- candidates[-b]
    utils <- utils[-b]
    if (b>1)
      utils[b-1] <- evalf.td(candidates[b-1],
                             closest.below(candidates[b-1], breaks),
                             closest.above(candidates[b-1], breaks),
                             v, class)
    if (b<=length(candidates))
      utils[b] <- evalf.td(candidates[b],
                           closest.below(candidates[b], breaks),
                           closest.above(candidates[b], breaks),
                           v, class)
    b <- which.max(utils)
  }
  breaks
}

## top-down discretization for a dataset
disc.topdown <- disc.all(disc.topdown1)


if (FALSE)
{

  # top-down discretization of the temperature attribute in the weatherc data
disc.topdown1(weatherc$temperature, weatherc$play, 3,
              evalf=function(b, bl, br, v, class) b)

  # top-down discretization for the weatherc data
disc.topdown(play~., weatherc, 3, evalf=function(b, bl, br, v, class) b)

}
