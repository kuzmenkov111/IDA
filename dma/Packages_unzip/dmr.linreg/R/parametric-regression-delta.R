## calculate parameter update based on given true and predicted values,
## gradient, and step-size using the delta rule for MSE minimization
delta.mse <- function(true.y, pred.y, gr, beta)
{ colSums(beta*rmm(gr, (true.y-pred.y))) }

if (FALSE)
{

  # gradient of the perfect representation function for f
grad.perf <- function(data, w)
{
  n <- ncol(data <- as.matrix(data))
  cbind(w[2*n+3]*(1-tanh(rowSums(cmm(data, w[1:n]))+w[n+1])^2)*data,
        w[2*n+3]*(1-tanh(rowSums(cmm(data, w[1:n]))+w[n+1])^2),
        w[2*n+4]*(1-tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2])^2)*data,
        w[2*n+4]*(1-tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2])^2),
        tanh(rowSums(cmm(data, w[1:n]))+w[n+1]),
        tanh(rowSums(cmm(data, w[(n+2):(2*n+1)]))+w[2*n+2]),
        1)
}

  # delta rule for the perfect model
delta.mse(prdat.train$f, predict(mod.perf, prdat.train[,1:4]),
          grad.perf(prdat.train[,1:4], w.perf), 0.1)
  # delta rule for the perfect model with modified target function values
delta.mse(prdat.train$f+1, predict(mod.perf, prdat.train[,1:4]),
          grad.perf(prdat.train[,1:4], w.perf), 0.1)

}
