## attribute selection search initialization
asel.init.none <- function(attributes) { character(0) }
asel.init.all <- function(attributes) { attributes }

  # attribute selection search initialization for the weather data
asel.init.none(names(weather)[-5])
asel.init.all(names(weather)[-5])
