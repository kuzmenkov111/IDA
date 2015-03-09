## for each value in v1 get the closest less value in v2 (or -Inf if there is none)
closest.below <- function(v1, v2)
{
  sapply(v1, function(v11) suppressWarnings(max(v2[v2<v11])))
}

## for each value in v1 get the closest greater value in v2 (or Inf if there is none)
closest.above <- function(v1, v2)
{
  sapply(v1, function(v11) suppressWarnings(min(v2[v2>v11])))
}

if (FALSE)
{

  # usage examples
closest.below(1:5, 1:10)
closest.below(6:10, 1:10)
closest.below(2*(1:4), 1:10)

closest.above(1:5, 1:10)
closest.above(6:10, 1:10)
closest.above(2*(1:4), 1:10)

}
