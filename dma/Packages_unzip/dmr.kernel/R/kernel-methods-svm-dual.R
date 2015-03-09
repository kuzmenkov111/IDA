## linear SVM parameter estimation using dual-form quadratic programming
## solvers: "solve.QP" or "ipop"
svm.linear.dual <- function(formula, data, svthres=1e-3, inf=1e3, solver="solve.QP")
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
                 Amat=matrix(c(cvec, diag(1, nrow(data))), nrow=nrow(data)),
                 bvec=rep(0, nrow(data)+1),
                 meq=1)
  else if (solver=="ipop")
    args <- list(c=rep(-1, nrow(data)),
                 H=dpmat*ccmat,
                 A=cvec,
                 b=0,
                 l=rep(0, nrow(data)),
                 u=rep(inf, nrow(data)),
                 r=0)
  else
    stop("Unknown solver: ", solver)

  qp <- do.call(solver, args)
  alpha <- if (solver=="solve.QP") qp$solution else if (solver=="ipop") qp@primal
  sv <- which(alpha>svthres)
  w <- c(colSums(rmm(amat[sv,], cvec[sv]*alpha[sv])))  # no intercept yet
  p0 <- predict.par(list(repf=repf.linear, w=c(w, 0)), data[,aind,drop=FALSE])
  w <- c(w, intercept=-(max(p0[cvec==-1])+min(p0[cvec==1]))/2)
  list(model=`class<-`(list(repf=repf.threshold(repf.linear), w=w), "par"), sv=sv)
}


if (FALSE)
{

  # estimate linear SVM model parameters
svm.d.ls <- svm.linear.dual(c~a1+a2+a3+a4, kmdat.train.ls)

  # misclassification error
err(predict(svm.d.ls$model, kmdat.train.ls[,1:4]), kmdat.train.ls$c)
err(predict(svm.d.ls$model, kmdat.test.ls[,1:4]), kmdat.test.ls$c)

}
