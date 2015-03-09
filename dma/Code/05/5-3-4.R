## estimate linear threshold model parameters using the OLS method
ols.threshold <- function(formula, data)
{
  class <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  amat <- cbind(as.matrix(data[,aind]), intercept=rep(1, nrow(data)))
  cvec <- 2*as.num0(data[[class]])-1
  `class<-`(list(repf=repf.threshold(repf.linear),
                 w=solve(t(amat)%*%amat, t(amat)%*%cvec)),
            "par")
}

  # least squares linear threshold for the artificial data
ols.th <- ols.threshold(c~., lcdat.train)
ols.th.ls <- ols.threshold(c~., lcdat.train.ls)

  # least squares linear threshold for the Pima Indians Diabetes data
pid.ols.th <- ols.threshold(diabetes~., pid.train)

  # training set error
err(predict(ols.th, lcdat.train[,1:4]), lcdat.train$c)
err(predict(ols.th.ls, lcdat.train.ls[,1:4]), lcdat.train.ls$c)
err(factor(predict(pid.ols.th, pid.train[,-9]),
           levels=0:1, labels=levels(pid.train$diabetes)),
    pid.train$diabetes)

  # test set error
err(predict(ols.th, lcdat.test[,1:4]), lcdat.test$c)
err(predict(ols.th.ls, lcdat.test.ls[,1:4]), lcdat.test.ls$c)
err(factor(predict(pid.ols.th, pid.test[,-9]),
           levels=0:1, labels=levels(pid.train$diabetes)),
    pid.test$diabetes)
