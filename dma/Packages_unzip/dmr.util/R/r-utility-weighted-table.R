## weighted frequency table
weighted.table <- function(v, ..., w=rep(1, length(v)), dnn=NULL)
{
  wtab <- as.table(replace(wt <- tapply(w, list(v, ...),
                                        function(w1) sum(w1)), is.na(wt), 0))
  names(dimnames(wtab)) <- dnn
  wtab
}


if (FALSE)
{

  # usage examples
weighted.table(weather$play)
weighted.table(weather$play, w=ifelse(weather$play=='yes', 2, 1))
weighted.table(weather$outlook, weather$play, dnn=c("outlook", "play"))
weighted.table(weather$outlook, weather$play, w=ifelse(weather$play=='yes', 2, 1))

}
