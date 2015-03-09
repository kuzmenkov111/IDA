## unit step function
ustep <- function(v, thres=0) { as.numeric(v>=thres) }


if (FALSE)
{

  # usage examples
ustep(seq(-1,1,0.25))
ustep(seq(-1,1,0.25), 0.5)

}
