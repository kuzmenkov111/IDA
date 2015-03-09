## minimum-cost rule for a single instance
mincostclas1 <- function(p, rho)
{ factor(which.min(rho%*%p), levels=1:length(p), labels=names(p)) }

## minimum-cost rule for multiple instances
mincostclas <- function(p, rho) { apply(p, 1, mincostclas1, rho) }

## generate a minimum-cost wrapper
mc.mincost <- function(alg, ppredf=predict)
{
  wrapped.alg <- function(formula, data, rho=NULL, ...)
  {
    list(model=alg(formula, data, ...), rho=rho)
  }

  wrapped.predict <- function(model, data, rho=NULL, ...)
  {
    mincostclas(ppredf(model$model, data, ...),
                if (is.null(rho)) model$rho else rho)
  }

  list(alg=wrapped.alg, predict=wrapped.predict)
}

  # minimum-cost wrapper around rpart
rpart.m <- mc.mincost(rpart)

  # minimum-cost wrapper around naiveBayes
naiveBayes.m <- mc.mincost(naiveBayes, ppredf=function(...) predict(..., type="r"))

  # decision trees with minimum-cost prediction
v.tree.m <- rpart.m$alg(Class~., v.train, v.rm, cp=0.025)
v01.tree.m <- rpart.m$alg(Class~., v01.train, v01.rm, cp=0.025)

  # naive Bayes with minimum-cost prediction
v.nb.m <- naiveBayes.m$alg(Class~., v.train, v.rm)
v01.nb.m <- naiveBayes.m$alg(Class~., v01.train, v01.rm)

  # mean misclassification cost with respect to the cost matrix
v.mc.m <- list(tree=mean.cost(rpart.m$predict(v.tree.m, v.test), v.test$Class, v.rm),
               nb=mean.cost(naiveBayes.m$predict(v.nb.m, v.test),
                            v.test$Class, v.rm))
v01.mc.m <- list(tree=mean.cost(rpart.m$predict(v01.tree.m, v01.test),
                                v01.test$Class, v01.rm),
                 nb=mean.cost(naiveBayes.m$predict(v01.nb.m, v01.test),
                              v01.test$Class, v01.rm))

# mean misclassification cost with respect to the per-class cost vector
v.mcc.m <- list(tree=mean.cost(rpart.m$predict(v.tree.m, v.test),
                               v.test$Class, v.rcm),
                nb=mean.cost(naiveBayes.m$predict(v.nb.m, v.test),
                             v.test$Class, v.rcm))

  # misclassification error
v.err.m <- list(tree=err(rpart.m$predict(v.tree.m, v.test), v.test$Class),
                nb=err(naiveBayes.m$predict(v.nb.m, v.test), v.test$Class))

v01.err.m <- list(tree=err(rpart.m$predict(v01.tree.m, v01.test), v01.test$Class),
                  nb=err(naiveBayes.m$predict(v01.nb.m, v01.test), v01.test$Class))

  # confusion matrix
confmat(rpart.m$predict(v.tree.m, v.test), v.test$Class)
confmat(naiveBayes.m$predict(v.nb.m, v.test), v.test$Class)

confmat(rpart.m$predict(v01.tree.m, v01.test), v01.test$Class)
confmat(naiveBayes.m$predict(v01.nb.m, v01.test), v01.test$Class)
