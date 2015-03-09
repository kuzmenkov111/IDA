## estimate linear threshold model parameters using the OLS method
## for data with discrete attributes
ols.threshold.disc <- function(formula, data)
{
  class <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  amat <- cbind(as.matrix(discode(~., data[,aind])), intercept=rep(1, nrow(data)))
  cvec <- 2*as.num0(data[[class]])-1
  `class<-`(list(repf=repf.disc(repf.threshold(repf.linear)),
                 w=solve(t(amat)%*%amat, t(amat)%*%cvec)),
            "par")
}


if (FALSE)
{

  # gradient descent for the weatherc data
w.gdl <-  gradient.descent(play~., weatherc, w=rep(0, 6),
                           repf=repf.disc(repf.threshold(repf.linear)),
                           grad=grad.disc(grad.threshold(grad.linear)),
                           delta=delta.mse, perf=err,
                           beta=1, batch=TRUE, eps=0.2)
err(factor(ustep(predict(w.gdl$model, weatherc[,1:4]), 0.5),
           levels=0:1, labels=c("no", "yes")),
    weatherc$play)

  # OLS for the weatherc data
w.ols <- ols.threshold.disc(play~., weatherc)
err(factor(ustep(predict(w.ols, weatherc[,1:4]), 0.5),
           levels=0:1, labels=c("no", "yes")),
    weatherc$play)

}
