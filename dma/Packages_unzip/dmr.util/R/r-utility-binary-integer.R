## convert an integer number to a binary vector, assuming the specified maximum value to determine t
int2binvec <- function(v, max=255) { as.integer(intToBits(v))[1:(floor(log2(max))+1)] }

## convert a binary vector to an integer number
binvec2int <- function(v) { packBits(as.integer(c(v, rep(0, 32-length(v)%%32))), type="i") }


if (FALSE)
{

  # usage examples
int2binvec(11, max=15)
int2binvec(11, max=63)

binvec2int(c(1, 1, 0, 1))
binvec2int(c(1, 1, 0, 1, 0))
binvec2int(c(1, 1, 0, 1, 1))

}
