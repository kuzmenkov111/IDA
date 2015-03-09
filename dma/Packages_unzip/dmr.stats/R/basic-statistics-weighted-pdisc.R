## weighted discrete probability distribution
weighted.pdisc <- function(v, ..., w=rep(1:length(v)))
{
  (count <- weighted.table(v, ..., w=w))/sum(count)
}


if (FALSE)
{

  # demonstration
weighted.pdisc(weather$outlook, w=ifelse(weather$play=="yes", 2, 1))
weighted.pdisc(weather$outlook, weather$temperature,
               w=ifelse(weather$play=="yes", 2, 1))

}
