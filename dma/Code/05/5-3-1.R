## threshold representation gradient
grad.threshold <- function(grad) { grad }

  # linear threshold gradient for the "perfect" parameter vector
grad.threshold(grad.linear)(lcdat.train[1:10,1:4], w.perf)
