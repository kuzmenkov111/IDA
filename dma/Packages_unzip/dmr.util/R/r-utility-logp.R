## log2(p) for p>=0
logp <- function(p) { ifelse(p > 0, log2(p), .Machine$double.min.exp) }


if (FALSE)
{

  # usage examples
logp(0)
logp(1)
logp(0.5)

}
