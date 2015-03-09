## kernel-based SVR parameter estimation using quadratic programming
## solvers: "solve.QP" or "ipop"
svr.kernel <- function(formula, data, eps=0.01,
                       kernel=kernel.linear, kernel.args=NULL,
                       cost=1, svthres=1e-3, solver="solve.QP")
{
  f <- y.var(formula)
  attributes <- x.vars(formula, data)
  aind <- names(data) %in% attributes

  fvec <- data[[f]]                   # target function vector
  amat <- as.matrix(data[,aind])      # attribute value matrix
  kmat <- do.call(kernel, c(list(amat), kernel.args)) # kernel matrix

  if (solver=="solve.QP")
    args <- list(Dmat=nearPD(rbind(cbind(kmat, -kmat), cbind(-kmat, kmat)))$mat,
                 dvec=c(fvec-eps, -fvec-eps),
                 Amat=matrix(c(rep(1, nrow(data)), rep(-1, nrow(data)),
                               diag(1, 2*nrow(data)), diag(-1, 2*nrow(data))),
                             nrow=2*nrow(data)),
                 bvec=c(0, rep(0, 2*nrow(data)), rep(-cost, 2*nrow(data))),
                 meq=1)
  else if (solver=="ipop")
    args <- list(c=c(-fvec+eps, fvec+eps),
                 H=rbind(cbind(kmat, -kmat), cbind(-kmat, kmat)),
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
  model <- list(coef=beta[sv], mat=amat[sv,,drop=FALSE],
                kernel=kernel, kernel.args=kernel.args, formula=formula)
  i <- which.min(abs(beta-cost/2))
  `class<-`(c(model,
              intercept=fvec[i]-unname(predict.kernel(c(model, intercept=0),
                                                      data[i,aind,drop=FALSE]))-
                          sign(beta[i])*eps),
            "svr.kernel")
}

## kernel-based SVR prediction
predict.svr.kernel <- predict.kernel

  # kernel-based SVR for f
svrf.kl <- svr.kernel(f~a1+a2+a3+a4, kmdat.train)
svrf.kp <- svr.kernel(f~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.polynomial, kernel.args=list(p=2, b=1))
svrf.kr <- svr.kernel(f~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.radial, kernel.args=list(gamma=0.02))
svrf.ks <- svr.kernel(f~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.sigmoid, kernel.args=list(gamma=0.2, b=0))

  # kernel-based SVR for g
svrg.kl <- svr.kernel(g~a1+a2+a3+a4, kmdat.train)
svrg.kp <- svr.kernel(g~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.polynomial, kernel.args=list(p=2, b=1))
svrg.kr <- svr.kernel(g~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.radial, kernel.args=list(gamma=0.1))
svrg.ks <- svr.kernel(g~a1+a2+a3+a4, kmdat.train,
                      kernel=kernel.sigmoid, kernel.args=list(gamma=0.02, b=-1))

  # kernel-based SVR for the Boston Housing data
bh.svr.kl <- svr.kernel(medv~., bh.std.train)
bh.svr.kr <- svr.kernel(medv~., bh.std.train,
                        kernel=kernel.radial, kernel.args=list(gamma=0.1))

  # training set MSE
mse(predict(svrf.kl, kmdat.train), kmdat.train$f)
mse(predict(svrf.kp, kmdat.train), kmdat.train$f)
mse(predict(svrf.kr, kmdat.train), kmdat.train$f)
mse(predict(svrf.ks, kmdat.train), kmdat.train$f)

mse(predict(svrg.kl, kmdat.train), kmdat.train$g)
mse(predict(svrg.kp, kmdat.train), kmdat.train$g)
mse(predict(svrg.kr, kmdat.train), kmdat.train$g)
mse(predict(svrg.ks, kmdat.train), kmdat.train$g)

mse(predict(bh.svr.kl, bh.std.train[,-13]), bh.std.train$medv)
mse(predict(bh.svr.kr, bh.std.train[,-13]), bh.std.train$medv)

  # test set MSE
mse(predict(svrf.kl, kmdat.test), kmdat.test$f)
mse(predict(svrf.kp, kmdat.test), kmdat.test$f)
mse(predict(svrf.kr, kmdat.test), kmdat.test$f)
mse(predict(svrf.ks, kmdat.test), kmdat.test$f)

mse(predict(svrg.kl, kmdat.test), kmdat.test$g)
mse(predict(svrg.kp, kmdat.test), kmdat.test$g)
mse(predict(svrg.kr, kmdat.test), kmdat.test$g)
mse(predict(svrg.ks, kmdat.test), kmdat.test$g)

mse(predict(bh.svr.kl, bh.std.test[,-13]), bh.std.test$medv)
mse(predict(bh.svr.kr, bh.std.test[,-13]), bh.std.test$medv)
