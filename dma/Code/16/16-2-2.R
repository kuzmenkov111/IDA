## linear SVM parameter estimation using primal-form quadratic programming
## solvers: "solve.QP" or "ipop"
svm.linear.prim <- function(formula, data, svthres=1e-9, inf=1e3, solver="solve.QP")
{
  class <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  cvec <- 2*as.num0(data[[class]])-1  # class vector using {-1, 1} labels
  amat <- cbind(as.matrix(data[,aind]), intercept=1)  # attribute value matrix

  if (solver=="solve.QP")
    args <- list(Dmat=nearPD(rbind(cbind(diag(sum(aind)), 0), 0))$mat,
                 dvec=rep(0, sum(aind)+1),
                 Amat=t(rmm(amat, cvec)),
                 bvec=rep(1, nrow(data)))
  else if (solver=="ipop")
    args <- list(c=rep(0, sum(aind)+1),
                 H=rbind(cbind(diag(sum(aind)), 0), 0),
                 A=rmm(amat, cvec),
                 b=rep(1, nrow(data)),
                 l=rep(-inf, sum(aind)+1),
                 u=rep(inf, sum(aind)+1),
                 r=rep(inf, nrow(data)))
  else stop("Unknown solver: ", solver)

  qp <- do.call(solver, args)
  w <- if (solver=="solve.QP") qp$solution else if (solver=="ipop") qp@primal
  sv <- unname(which(cvec*predict.par(list(repf=repf.linear, w=w),
                                      data[,aind,drop=FALSE])<=1+svthres))
  list(model=`class<-`(list(repf=repf.threshold(repf.linear), w=w), "par"), sv=sv)
}

  # estimate linear SVM model parameters
svm.p.ls <- svm.linear.prim(c~a1+a2+a3+a4, kmdat.train.ls)

  # misclassification error
err(predict(svm.p.ls$model, kmdat.train.ls[,1:4]), kmdat.train.ls$c)
err(predict(svm.p.ls$model, kmdat.test.ls[,1:4]), kmdat.test.ls$c)
