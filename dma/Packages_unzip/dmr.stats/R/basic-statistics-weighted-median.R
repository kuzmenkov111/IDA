weighted.median <- function(v, w=rep(1, length(v)))
{
  v <- v[ord <- order(v)]
  w <- w[ord]
  tw <- (sw <- cumsum(w))[length(sw)]
  mean(v[which(sw>=0.5*tw & tw-shift.right(sw, 0)>=0.5*tw)])
}


if (FALSE)
{
  # demonstration
weighted.median(weatherc$temperature, ifelse(weatherc$play=="yes", 5, 1))
median(c(weatherc$temperature[weatherc$play=="no"],
         rep(weatherc$temperature[weatherc$play=="yes"], 5)))
weighted.median(weatherc$temperature, ifelse(weatherc$play=="yes", 0.2, 1))
median(c(weatherc$temperature[weatherc$play=="yes"],
         rep(weatherc$temperature[weatherc$play=="no"], 5)))

}
