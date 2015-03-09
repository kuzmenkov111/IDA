## a naive application of OLS to a generalized linear representation
ols.gen <- function(formula, data, link=function(v) v, link.inv=function(v) v)
{
  f <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  amat <- cbind(as.matrix(data[,aind]), intercept=rep(1, nrow(data)))
  fvec <- link(data[[f]])
  `class<-`(list(repf=repf.gen(link.inv), w=solve(t(amat)%*%amat, t(amat)%*%fvec)),
            "par")
}

  # perfect link function for f2
link2 <- function(v) { 10*atanh(v) }

  # estimate of generalized linear model parameters for f2
ols2g <- ols.gen(f2~a1+a2+a3+a4, lrdat.train, link=link2, link.inv=link2.inv)
  # test set error
mse(predict(ols2g, lrdat.test[,1:4]), lrdat.test$f2)
