## m-variance that incorporates m fictitious values with a specified variance s02
mvar <- function(v, m=2, m0=mean(v), s02=var(v))
{ (sum((v-mmean(v, m, m0))^2)+max(m-1, 0)*s02)/max(length(v)+m-2, 1)  }

  # demonstration
mvar(weatherr$playability)
mvar(weatherr$playability, m=0)
mvar(weatherr$playability, s02=0.05)
mvar(weatherr$playability, m=5, s02=0.05)
mvar(weatherr$playability[weatherr$temperature<25], m=0)
mvar(weatherr$playability[weatherr$temperature<25],
     m0=mean(weatherr$playability), s02=var(weatherr$playability))
