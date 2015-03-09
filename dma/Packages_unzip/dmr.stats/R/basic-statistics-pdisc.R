pdisc <- function(v, ...) { (count <- table(v, ..., dnn=NULL))/sum(count) }


if (FALSE)
{

  # demonstration
pdisc(weather$outlook)
pdisc(weather$outlook, weather$temperature)

}
