## generate an instance-weighting cost-sensitive wrapper
mc.weight <- function(alg, predf=predict)
{
  wrapped.alg <- function(formula, data, rho, ...)
  {
    class <- y.var(formula)
    w <- rho[data[[class]]]
    do.call(alg, list(formula, data, weights=w, ...))
  }

  list(alg=wrapped.alg, predict=predf)
}

  # weighting wrapper around rpart
rpart.w <- mc.weight(rpart, predf=function(...) predict(..., type="c"))

  # decision trees with instance weighting
v.tree.w <- rpart.w$alg(Class~., v.train, v.rc)
v01.tree.w <- rpart.w$alg(Class~., v01.train, v01.rc)

  # mean misclassification cost with respect to the cost matrix
v.mc.w <- list(tree=mean.cost(rpart.w$predict(v.tree.w, v.test), v.test$Class, v.rm))
v01.mc.w <- list(tree=mean.cost(rpart.w$predict(v01.tree.w, v01.test),
                                v01.test$Class, v01.rm))

# mean misclassification cost with respect to the per-class cost vector
v.mcc.w <- list(tree=mean.cost(rpart.w$predict(v.tree.w, v.test),
                               v.test$Class, v.rcm))

  # misclassification error
v.err.w <- list(tree=err(rpart.w$predict(v.tree.w, v.test), v.test$Class))
v01.err.w <- list(tree=err(rpart.w$predict(v01.tree.w, v01.test), v01.test$Class))

  # confusion matrix
confmat(rpart.w$predict(v.tree.w, v.test), v.test$Class)
confmat(rpart.w$predict(v01.tree.w, v01.test), v01.test$Class)
