## linear SVR parameter estimation using quadratic programming
## solvers: "solve.QP" or "ipop"
svr.linear <- function(formula, data, eps=0.01, cost=1, svthres=1e-3,
                       solver="solve.QP")
{
  f <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  fvec <- data[[f]]                   # target function vector
  amat <- as.matrix(data[,aind])      # attribute value matrix
  dpmat <- amat%*%t(amat)             # dot product matrix

  if (solver=="solve.QP")
    args <- list(Dmat=nearPD(rbind(cbind(dpmat, -dpmat), cbind(-dpmat, dpmat)))$mat,
                 dvec=c(fvec-eps, -fvec-eps),
                 Amat=matrix(c(rep(1, nrow(data)), rep(-1, nrow(data)),
                               diag(1, 2*nrow(data)), diag(-1, 2*nrow(data))),
                             nrow=2*nrow(data)),
                 bvec=c(0, rep(0, 2*nrow(data)), rep(-cost, 2*nrow(data))),
                 meq=1)
  else if (solver=="ipop")
    args <- list(c=c(-fvec+eps, fvec+eps),
                 H=rbind(cbind(dpmat, -dpmat), cbind(-dpmat, dpmat)),
                 A=c(rep(1, nrow(data)), rep(-1, nrow(data))),
                 b=0,
                 l=rep(0, 2*nrow(data)),
                 u=rep(cost, 2*nrow(data)),
                 r=0)
  else
    stop("Unknown solver: ", solver)

  qp <- do.call(solver, args)
  alpha <- if (solver=="solve.QP") qp$solution else if (solver=="ipop") qp@primal
  beta <- alpha[1:nrow(data)]-alpha[(nrow(data)+1):(2*nrow(data))]
  sv <- which(abs(beta)>svthres)
  w <- c(colSums(rmm(amat[sv,], beta[sv])))  # no intercept yet
  i <- which.min(abs(beta-cost/2))
  w <- c(w, intercept=fvec[i]-unname(predict.par(list(repf=repf.linear, w=c(w, 0)),
                                                 data[i,aind,drop=FALSE]))-
                        sign(beta[i])*eps)
  list(model=`class<-`(list(repf=repf.linear, w=w), "par"), sv=sv)
}

  # linear SVR for f
svrf <- svr.linear(f~a1+a2+a3+a4, kmdat.train)
svrf.e1 <- svr.linear(f~a1+a2+a3+a4, eps=1, kmdat.train)
svrf.c01 <- svr.linear(f~a1+a2+a3+a4, cost=0.1, kmdat.train)

  # linear SVR for g
svrg <- svr.linear(g~a1+a2+a3+a4, kmdat.train)
svrg.e1 <- svr.linear(g~a1+a2+a3+a4, eps=1, kmdat.train)
svrg.c01 <- svr.linear(g~a1+a2+a3+a4, cost=0.1, kmdat.train)

  # linear SVR for the Boston Housing data
bh.svr <- svr.linear(medv~., bh.std.train)
bh.svr.e1 <- svr.linear(medv~., eps=1, bh.std.train)
bh.svr.c01 <- svr.linear(medv~., cost=0.1, bh.std.train)

  # training set MSE
mse(predict(svrf$model, kmdat.train[,1:4]), kmdat.train$f)
mse(predict(svrf.e1$model, kmdat.train[,1:4]), kmdat.train$f)
mse(predict(svrf.c01$model, kmdat.train[,1:4]), kmdat.train$f)

mse(predict(svrg$model, kmdat.train[,1:4]), kmdat.train$g)
mse(predict(svrg.e1$model, kmdat.train[,1:4]), kmdat.train$g)
mse(predict(svrg.c01$model, kmdat.train[,1:4]), kmdat.train$g)

mse(predict(bh.svr$model, bh.std.train[,-13]), bh.std.train$medv)
mse(predict(bh.svr.e1$model, bh.std.train[,-13]), bh.std.train$medv)
mse(predict(bh.svr.c01$model, bh.std.train[,-13]), bh.std.train$medv)

  # test set MSE
mse(predict(svrf$model, kmdat.test[,1:4]), kmdat.test$f)
mse(predict(svrf.e1$model, kmdat.test[,1:4]), kmdat.test$f)
mse(predict(svrf.c01$model, kmdat.test[,1:4]), kmdat.test$f)

mse(predict(svrg$model, kmdat.test[,1:4]), kmdat.test$g)
mse(predict(svrg.e1$model, kmdat.test[,1:4]), kmdat.test$g)
mse(predict(svrg.c01$model, kmdat.test[,1:4]), kmdat.test$g)

mse(predict(bh.svr$model, bh.std.test[,-13]), bh.std.test$medv)
mse(predict(bh.svr.e1$model, bh.std.test[,-13]), bh.std.test$medv)
mse(predict(bh.svr.c01$model, bh.std.test[,-13]), bh.std.test$medv)
