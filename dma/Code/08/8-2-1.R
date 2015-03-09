## parametric regression prediction for a given model and dataset
predict.par <-  function(model, data) { model$repf(data, model$w) }

  # perfect representation function for f4
repf.perf4 <- function(data, w)
{
  w[2*(n <- ncol(data))+3]*tanh(rowSums(cmm(data, w[1:n]))+w[n+1]) +
    w[2*n+4]*tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2]) + w[2*n+5]
}

  # perfect parameters for f4
w.perf4 <- c(1, -2, 3, -1, 1, -2, 3, -2, 1, -1, 2, -3, 2)
  # perfect model for f4
mod.perf4 <- `class<-`(list(w=w.perf4, repf=repf.perf4), "par")
  # test set error
mse(predict(mod.perf4, lrdat.test[,1:4]), lrdat.test$f4)
