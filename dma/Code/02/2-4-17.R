qd <- function(v) { unname(diff(q <- quantile(v, c(0.25, 0.75)))/sum(q)) }

  # demonstration
qd(weatherc$temperature)
