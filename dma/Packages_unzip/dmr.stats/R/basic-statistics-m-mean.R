## m-mean that incorporates m fictitious values with a specified mean m0
mmean <- function(v, m=2, m0=mean(v)) { (sum(v)+m*m0)/(length(v)+m) }

if (FALSE)
{

  # demonstration
mmean(weatherr$playability)
mmean(weatherr$playability, m=0)
mmean(weatherr$playability, m0=0.5)
mmean(weatherr$playability, 5, 0.5)
mmean(weatherr$playability[weatherr$temperature<25], m=0)
mmean(weatherr$playability[weatherr$temperature<25], m0=mean(weatherr$playability))

}
