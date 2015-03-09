## linear soft-margin SVM parameter estimation using quadratic programming
## solvers: "solve.QP" or "ipop"
svm.linear <- function(formula, data, cost=1, svthres=1e-3, solver="solve.QP")
{
  class <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  cvec <- 2*as.num0(data[[class]])-1  # class vector using {-1, 1} labels
  ccmat <- outer(cvec, cvec)          # class-class product matrix
  amat <- as.matrix(data[,aind])      # attribute value matrix
  dpmat <- amat%*%t(amat)             # dot product matrix

  if (solver=="solve.QP")
    args <- list(Dmat=nearPD(dpmat*ccmat)$mat,
                 dvec=rep(1, nrow(data)),
                 Amat=matrix(c(cvec, diag(1, nrow(data)), diag(-1, nrow(data))),
                             nrow=nrow(data)),
                 bvec=c(0, rep(0, nrow(data)), rep(-cost, nrow(data))),
                 meq=1)
  else if (solver=="ipop")
    args <- list(c=rep(-1, nrow(data)),
                 H=dpmat*ccmat,
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
  w <- c(colSums(rmm(amat[sv,], cvec[sv]*alpha[sv])))  # no intercept yet
  i <- which.min(abs(alpha-cost/2))
  w <- c(w, intercept=cvec[i]-unname(predict.par(list(repf=repf.linear, w=c(w, 0)),
                                                 data[i,aind,drop=FALSE])))
  list(model=`class<-`(list(repf=repf.threshold(repf.linear), w=w), "par"), sv=sv)
}

  # linear SVM for the artificial data
svm.s <- svm.linear(c~a1+a2+a3+a4, kmdat.train)
svm.s.ls <- svm.linear(c~a1+a2+a3+a4, kmdat.train.ls)

svm.s.01 <- svm.linear(c~a1+a2+a3+a4, kmdat.train, cost=0.1)
svm.s.ls.01 <- svm.linear(c~a1+a2+a3+a4, kmdat.train.ls, cost=0.1)

svm.s.10 <- svm.linear(c~a1+a2+a3+a4, kmdat.train, cost=10)
svm.s.ls.10 <- svm.linear(c~a1+a2+a3+a4, kmdat.train.ls, cost=10)

  # linear SVM for the Pima Indians Diabetes data
pid.svm.s <- svm.linear(diabetes~., pid.std.train)
pid.svm.s.01 <- svm.linear(diabetes~., pid.std.train, cost=0.1)
pid.svm.s.10 <- svm.linear(diabetes~., pid.std.train, cost=10)

  # training set misclassification error
err(predict(svm.s$model, kmdat.train[,1:4]), kmdat.train$c)
err(predict(svm.s.01$model, kmdat.train[,1:4]), kmdat.train$c)
err(predict(svm.s.10$model, kmdat.train[,1:4]), kmdat.train$c)

err(predict(svm.s.ls$model, kmdat.train.ls[,1:4]), kmdat.train.ls$c)
err(predict(svm.s.ls.01$model, kmdat.train.ls[,1:4]), kmdat.train.ls$c)
err(predict(svm.s.ls.10$model, kmdat.train.ls[,1:4]), kmdat.train.ls$c)

err(factor(predict(pid.svm.s$model, pid.std.train[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.std.train$diabetes)
err(factor(predict(pid.svm.s.01$model, pid.std.train[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.std.train$diabetes)
err(factor(predict(pid.svm.s.10$model, pid.std.train[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.std.train$diabetes)

  # test set misclassification error
err(predict(svm.s$model, kmdat.test[,1:4]), kmdat.test$c)
err(predict(svm.s.01$model, kmdat.test[,1:4]), kmdat.test$c)
err(predict(svm.s.10$model, kmdat.test[,1:4]), kmdat.test$c)

err(predict(svm.s.ls$model, kmdat.test.ls[,1:4]), kmdat.test.ls$c)
err(predict(svm.s.ls.01$model, kmdat.test.ls[,1:4]), kmdat.test.ls$c)
err(predict(svm.s.ls.10$model, kmdat.test.ls[,1:4]), kmdat.test.ls$c)

err(factor(predict(pid.svm.s$model, pid.std.test[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.test$diabetes)
err(factor(predict(pid.svm.s.01$model, pid.std.test[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.test$diabetes)
err(factor(predict(pid.svm.s.10$model, pid.std.test[,-9]),
           levels=0:1, labels=levels(pid.std.train$diabetes)),
    pid.test$diabetes)
