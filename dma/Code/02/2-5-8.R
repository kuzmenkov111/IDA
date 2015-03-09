bs.t.test <- function(v, v01)
{
  m <- unname(tapply(v, v01, mean))
  s2 <- unname(tapply(v, v01, var))
  cn <- unname(tapply(v, v01, length))
  sp <- sqrt((s2[1]*(cn[1]-1)+s2[2]*(cn[2]-1))/(sum(cn)-2))

  ts <- (m[1]-m[2])/(sp*sqrt(sum(1/cn)))
  list(statistic=ts, p.value=2*(1-pt(abs(ts), sum(cn)-2)))
}

  # demonstration
bs.t.test(weatherc$temperature, weatherc$play)
t.test(temperature~play, weatherc, var.equal=TRUE)
