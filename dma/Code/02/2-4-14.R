relsd <- function(v) { abs(varcoef(v)) }

  # demonstration
relsd(weatherr$playability)
relsd(-weatherr$playability)
