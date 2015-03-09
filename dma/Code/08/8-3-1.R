## gradient of the linear representation function
grad.linear <- function(data, w) { cbind(data, 1) }

  # gradient for the first 10 instances
grad.linear(lrdat.train[1:10,1:4], rep(0, 5))
