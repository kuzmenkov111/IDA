## enhanced representation function
repf.enh <- function(enhance, repf=repf.linear)
{ function(data, w) { repf(enhance(data), w) } }

## enhanced representation function gradient
grad.enh <- function(enhance, grad=grad.linear)
{ function(data, w) { grad(enhance(data), w) } }

## estimate linear model parameters using the OLS method
## with enhanced representation
ols.enh <- function(formula, data, enhance=function(data) data)
{
  f <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  amat <- cbind(as.matrix(enhance(data[,aind])), intercept=rep(1, nrow(data)))
  fvec <- data[[f]]
  `class<-`(list(repf=repf.enh(enhance), w=solve(t(amat)%*%amat, t(amat)%*%fvec)),
            "par")
}


if (FALSE)
{

  # perfect representation enhancement for f3
enhance3 <- function(data)
{
  cbind(data, sq=data^2)
}

  # gradient descent estimation for f3
gd3e <- gradient.descent(f3~a1+a2+a3+a4, lrdat.train, w=rep(0, 9),
                         repf=repf.enh(enhance3), grad=grad.enh(enhance3),
                         beta=0.001, eps=0.005)
  # test set error
mse(predict(gd3e$model, lrdat.test[,1:4]), lrdat.test$f3)

  # OLS estimation for f3
ols3e <- ols.enh(f3~a1+a2+a3+a4, lrdat.train, enhance3)
  # test set error
mse(predict(ols3e, lrdat.test[,1:4]), lrdat.test$f3)

}
