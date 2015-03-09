## get character-encoded attribute type information ("c" -- continuous, "d" -- discrete)
attr.type <- function(...)
{
  attr.type1 <- function(a) ifelse(is.numeric(a), "c", "d")
  paste(sapply(data.frame(...), attr.type1), collapse="")
}


if (FALSE)
{

  # usage examples
attr.type(weatherr$outlook, weatherr$playability)
attr.type(weatherr)

}
