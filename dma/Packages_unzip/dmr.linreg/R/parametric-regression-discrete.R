## representation function wrapper to handle discrete attributes
repf.disc <- function(repf)
{ function(data, w) { repf(discode(~., data, b=c(-1,1)), w) } }

## representation function gradient wrapper to handle discrete attributes
grad.disc <- function(grad)
{ function(data, w) { grad(discode(~., data, b=c(-1,1)), w) } }


if (FALSE)
{

  # simple representation function for the weatherr data
repf.sigmoid <- function(data, w)
{ 0.5*(tanh(rowSums(cmm(data, w[1:(n <- ncol(data))])) + w[n+1])+1) }

  # and its gradient
grad.sigmoid <- function(data, w)
{ 0.5*(1-tanh(rowSums(cmm(data, w[1:(n <- ncol(data))])) + w[n+1])^2)*cbind(data, 1) }

  # gradient descent for the weatherr data
w.gd <- gradient.descent(playability~., weatherr, w=runif(6, min=-0.01, max=0.01),
                         repf=repf.disc(repf.sigmoid), grad=grad.disc(grad.sigmoid),
                         beta=0.0001, eps=0.005, niter=10000)
mse(predict(w.gd$model, weatherr[,1:4]), weatherr$playability)

}
