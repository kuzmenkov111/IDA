## kernel-based soft-margin SVM parameter estimation using quadratic programming
## solvers: "solve.QP" or "ipop"
svm.kernel <- function(formula, data, kernel=kernel.linear, kernel.args=NULL,
                       cost=1, svthres=1e-3, solver="solve.QP")
{
  class <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  cvec <- 2*as.num0(data[[class]])-1  # class vector using {-1, 1} labels
  ccmat <- outer(cvec, cvec)          # class-class product matrix
  amat <- as.matrix(data[,aind])      # attribute value matrix
  kmat <- do.call(kernel, c(list(amat), kernel.args)) # kernel matrix

  if (solver=="solve.QP")
    args <- list(Dmat=nearPD(kmat*ccmat)$mat,
                 dvec=rep(1, nrow(data)),
                 Amat=matrix(c(cvec, diag(1, nrow(data)), diag(-1, nrow(data))),
                             nrow=nrow(data)),
                 bvec=c(0, rep(0, nrow(data)), rep(-cost, nrow(data))),
                 meq=1)
  else if (solver=="ipop")
    args <- list(c=rep(-1, nrow(data)),
                 H=kmat*ccmat,
                 A=cvec,
                 b=0,
                 l=rep(0, nrow(data)),
                 u=rep(cost, nrow(data)),
                 r=0)
  else
    stop("Unknown solver: ", solver)

  qp <- do.call(solver, args)
  alpha <- if (solver=="solve.QP") qp$solution else if (solver=="ipop") qp@primal
  sv <- which(alpha>svthres)
  model <- list(coef=cvec[sv]*alpha[sv], mat=amat[sv,,drop=FALSE],
                kernel=kernel, kernel.args=kernel.args, formula=formula)
  i <- which.min(abs(alpha-cost/2))
  'class<-'(c(model, intercept=cvec[i]-
                                 unname(predict.kernel(c(model, intercept=0),
                                                       data[i,aind,drop=FALSE]))),
            "svm.kernel")
}

## kernel-based SVM prediction
predict.svm.kernel <- function(model, data)
{
  ustep(predict.kernel(model, data))
}


if (FALSE)
{

  # kernel-based SVM for the artificial data
svm.kl <- svm.kernel(c~a1+a2+a3+a4, kmdat.train)
svm.kp <- svm.kernel(c~a1+a2+a3+a4, kmdat.train,
                     kernel=kernel.polynomial, kernel.args=list(p=2, b=1))
svm.kr <- svm.kernel(c~a1+a2+a3+a4, kmdat.train,
                     kernel=kernel.radial, kernel.args=list(gamma=0.5))
svm.ks <- svm.kernel(c~a1+a2+a3+a4, kmdat.train,
                     kernel=kernel.sigmoid, kernel.args=list(gamma=0.04,b=-0.8))

  # kernel-based SVM for the Pima Indians Diabetes
pid.svm.kl <- svm.kernel(diabetes~., pid.std.train)
pid.svm.kr <- svm.kernel(diabetes~., pid.std.train,
                         kernel=kernel.radial, kernel.args=list(gamma=0.1))

  # training set misclassification error
err(predict(svm.kl, kmdat.train), kmdat.train$c)
err(predict(svm.kp, kmdat.train), kmdat.train$c)
err(predict(svm.kr, kmdat.train), kmdat.train$c)
err(predict(svm.ks, kmdat.train), kmdat.train$c)

err(factor(predict(pid.svm.kl, pid.std.train[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.std.train$diabetes)
err(factor(predict(pid.svm.kr, pid.std.train[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.std.train$diabetes)

  # test set misclassification error
err(predict(svm.kl, kmdat.test), kmdat.test$c)
err(predict(svm.kp, kmdat.test), kmdat.test$c)
err(predict(svm.kr, kmdat.test), kmdat.test$c)
err(predict(svm.ks, kmdat.test), kmdat.test$c)

err(factor(predict(pid.svm.kl, pid.std.test[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.test$diabetes)
err(factor(predict(pid.svm.kr, pid.std.test[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.test$diabetes)


}
