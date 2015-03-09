## generate an instance-resampling cost-sensitive wrapper
mc.resample <- function(alg, predf=predict)
{
  wrapped.alg <- function(formula, data, rho, ...)
  {
    class <- y.var(formula)
    w <- rho[data[[class]]]
    rs <- na.omit(c(rep(1:nrow(data), floor(w)),
                    ifelse(runif(nrow(data))<=w-floor(w), 1:nrow(data), NA)))
    do.call(alg, list(formula, data[rs,], ...))
  }

  list(alg=wrapped.alg, predict=predf)
}


if (FALSE)
{

  # resampling wrapper around rpart
rpart.s <- mc.resample(rpart, predf=function(...) predict(..., type="c"))

  # resampling wrapper around naiveBayes
naiveBayes.s <- mc.resample(naiveBayes)

  # decision trees with instance resampling
v.tree.s <- rpart.s$alg(Class~., v.train, v.rc)
v01.tree.s <- rpart.s$alg(Class~., v01.train, v01.rc)

  # naive Bayes with instance resampling
v.nb.s <- naiveBayes.s$alg(Class~., v.train, v.rc)
v01.nb.s <- naiveBayes.s$alg(Class~., v01.train, v01.rc)

  # mean misclassification cost with respect to the cost matrix
v.mc.s <- list(tree=mean.cost(rpart.s$predict(v.tree.s, v.test), v.test$Class, v.rm),
               nb=mean.cost(naiveBayes.s$predict(v.nb.s, v.test),
                            v.test$Class, v.rm))
v01.mc.s <- list(tree=mean.cost(rpart.s$predict(v01.tree.s, v01.test),
                                v01.test$Class, v01.rm),
                 nb=mean.cost(naiveBayes.s$predict(v01.nb.s, v01.test),
                              v01.test$Class, v01.rm))

# mean misclassification cost with respect to the per-class cost vector
v.mcc.s <- list(tree=mean.cost(rpart.s$predict(v.tree.s, v.test),
                               v.test$Class, v.rcm),
                nb=mean.cost(naiveBayes.s$predict(v.nb.s, v.test),
                             v.test$Class, v.rcm))

  # misclassification error
v.err.s <- list(tree=err(rpart.s$predict(v.tree.s, v.test), v.test$Class),
                nb=err(naiveBayes.s$predict(v.nb.s, v.test), v.test$Class))
v01.err.s <- list(tree=err(rpart.s$predict(v01.tree.s, v01.test), v01.test$Class),
                  nb=err(naiveBayes.s$predict(v01.nb.s, v01.test), v01.test$Class))

  # confusion matrix
confmat(rpart.s$predict(v.tree.s, v.test), v.test$Class)
confmat(naiveBayes.s$predict(v.nb.s, v.test), v.test$Class)

confmat(rpart.s$predict(v01.tree.s, v01.test), v01.test$Class)
confmat(naiveBayes.s$predict(v01.nb.s, v01.test), v01.test$Class)

}
