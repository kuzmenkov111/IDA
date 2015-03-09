## forward attribute selection search next state generation
asel.next.forward <- function(subset, attributes)
{
  lapply(setdiff(attributes, subset), function(a) c(subset, a))
}

## backward attribute selection search next state generation
asel.next.backward <- function(subset, attributes)
{
  if (length(subset)>1)
    lapply(1:length(subset), function(i) subset[-i])
  else
    list()
}

  # attribute selection next state generation for the weather data
asel.next.forward(c("outlook", "humidity"), names(weather)[-5])
asel.next.backward(c("outlook", "humidity"), names(weather)[-5])
