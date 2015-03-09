## parametric regression prediction for a given model and dataset
predict.par <-  function(model, data) { model$repf(data, model$w) }


if (FALSE)
{

  # perfect representation function for f
repf.perf <- function(data, w)
{
  w[2*(n <- ncol(data))+3]*tanh(rowSums(cmm(data, w[1:n]))+w[n+1]) +
    w[2*n+4]*tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2]) + w[2*n+5]
}

  # perfect parameters for f
w.perf <- c(1, -2, 3, -1, 1, -2, 3, -2, 1, -1, 2, -3, 2)
  # perfect model for f
mod.perf <- `class<-`(list(w=w.perf, repf=repf.perf), "par")
  # test set error
mse(predict(mod.perf, prdat.test[,1:4]), prdat.test$f)

}
