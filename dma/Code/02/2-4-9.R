## variance that returns 0 for 1-element vectors and NaN for empty vectors
var1 <- function(v) { switch(min(length(v), 2)+1, NaN, 0, var(v)) }

  # demonstration
var1(1:2)
var1(1)
var1(weatherr$temperature[weatherr$playability<0.75])
var1(weatherr$temperature[weatherr$playability>=0.75])
var1(weatherr$temperature[weatherr$playability>=0.8])
