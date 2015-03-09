## get factor levels as a factor of the same levels
flevels <- function(v) { factor(levels(v), levels=levels(v)) }


if (FALSE)
{

  # usage examples
flevels(weather$outlook)
flevels(weather$outlook)[as.integer(weather$outlook)]==weather$outlook

}
