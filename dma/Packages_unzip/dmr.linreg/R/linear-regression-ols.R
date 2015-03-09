## estimate linear model parameters using the OLS method
ols <- function(formula, data)
{
  f <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  amat <- cbind(as.matrix(data[,aind]), intercept=rep(1, nrow(data)))
  fvec <- data[[f]]
  `class<-`(list(repf=repf.linear, w=solve(t(amat)%*%amat, t(amat)%*%fvec)), "par")
}


if (FALSE)
{

  # estimate linear model parameters for f1
ols1 <- ols(f1~a1+a2+a3+a4, lrdat.train)
  # test set error
mse(predict(ols1, lrdat.test[,1:4]), lrdat.test$f1)

}
