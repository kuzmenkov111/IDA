## weighted variance that returns 0 for 1-element vectors and NaN for empty vectors
weighted.var1 <- function(v, w=rep(1, length(w)))
{ switch(min(length(v), 2)+1, NaN, 0, weighted.var(v, w)) }

  # demonstration
weighted.var1(1:2, 1:2)
weighted.var1(1, 2)
weighted.var1(weatherr$temperature[weatherr$playability<0.75],
              weatherr$playability[weatherr$playability<0.75])
weighted.var1(weatherr$temperature[weatherr$playability>=0.75],
              weatherr$playability[weatherr$playability>=0.75])
weighted.var1(weatherr$temperature[weatherr$playability>=0.8],
              weatherr$playability[weatherr$playability>=0.8])
