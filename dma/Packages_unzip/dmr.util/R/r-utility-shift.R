## return vector v shifted left, with the supplied new last value
shift.left <- function(v, last=NA) { c(v[2:length(v)], last) }

## return vector v shifted right, with the supplied new first value
shift.right <- function(v, first=NA) { c(first, v[1:(length(v)-1)]) }


if (FALSE)
{

  # usage examples
shift.left(1:10)
shift.right(1:10)

}
