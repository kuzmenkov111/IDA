## symmetric uncertainty for discrete vectors
symunc <- function(v1, v2)
{
  2*mutinfo(v1, v2)/(entropy(v1)+entropy(v2))
}


if (FALSE)
{

  # demonstration
symunc(weather$outlook, weather$temperature)
symunc(weather$outlook, weather$play)

}
