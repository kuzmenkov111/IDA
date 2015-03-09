## generate an instance-relabeling cost-sensitive wrapper
mc.relabel <- function(alg, palg=alg, pargs=NULL, predf=predict, ppredf=predict)
{
  wrapped.alg <- function(formula, data, rho, ...)
  {
    class <- y.var(formula)
    model <- do.call(palg, c(list(formula, data), pargs))
    prob <- ppredf(model, data)
    data[[class]] <- mincostclas(prob, rho)
    alg(formula, data, ...)
  }

  list(alg=wrapped.alg, predict=predf)
}

  # relabeling wrapper around rpart
rpart.l <- mc.relabel(rpart, pargs=list(cp=0.025),
                      predf=function(...) predict(..., type="c"))

  # relabeling wrapper around rpart using bagging for probability estimation
rpart.bagg.l <- mc.relabel(rpart, bagging,
                           pargs=list(control=rpart.control(cp=0.025)),
                           predf=function(...) predict(..., type="c"),
                           ppredf=function(...) predict(..., type="p",
                                                             aggregation="a"))

  # relabeling wrapper around naiveBayes
naiveBayes.l <- mc.relabel(naiveBayes, ppredf=function(...) predict(..., type="r"))

  # decision trees with instance relabeling
v.tree.l <- rpart.l$alg(Class~., v.train, v.rm)
v.tree.bagg.l <- rpart.bagg.l$alg(Class~., v.train, v.rm)
v01.tree.l <- rpart.l$alg(Class~., v01.train, v01.rm)
v01.tree.bagg.l <- rpart.bagg.l$alg(Class~., v01.train, v01.rm)

  # naive Bayes with instance relabeling
v.nb.l <- naiveBayes.l$alg(Class~., v.train, v.rm)
v01.nb.l <- naiveBayes.l$alg(Class~., v01.train, v01.rm)

  # mean misclassification cost with respect to the cost matrix
v.mc.l <- list(tree=mean.cost(rpart.l$predict(v.tree.l, v.test), v.test$Class, v.rm),
               tree.bagg=mean.cost(rpart.bagg.l$predict(v.tree.bagg.l, v.test),
                                   v.test$Class, v.rm),
               nb=mean.cost(naiveBayes.l$predict(v.nb.l, v.test),
                            v.test$Class, v.rm))
v01.mc.l <- list(tree=mean.cost(rpart.l$predict(v01.tree.l, v01.test),
                                v01.test$Class, v01.rm),
                 tree.bagg=mean.cost(rpart.bagg.l$predict(v01.tree.bagg.l, v01.test),
                                     v01.test$Class, v01.rm),
                 nb=mean.cost(naiveBayes.l$predict(v01.nb.l, v01.test),
                              v01.test$Class, v01.rm))

# mean misclassification cost with respect to the per-class cost vector
v.mcc.l <- list(tree=mean.cost(rpart.l$predict(v.tree.l, v.test),
                               v.test$Class, v.rcm),
                tree.bagg=mean.cost(rpart.bagg.l$predict(v.tree.bagg.l, v.test),
                                    v.test$Class, v.rcm),
                nb=mean.cost(naiveBayes.l$predict(v.nb.l, v.test),
                             v.test$Class, v.rcm))

  # misclassification error
v.err.l <- list(tree=err(rpart.l$predict(v.tree.l, v.test), v.test$Class),
                tree.bagg=err(rpart.bagg.l$predict(v.tree.bagg.l, v.test),
                              v.test$Class),
                nb=err(naiveBayes.l$predict(v.nb.l, v.test), v.test$Class))
v01.err.l <- list(tree=err(rpart.l$predict(v01.tree.l, v01.test), v01.test$Class),
                  tree.bagg=err(rpart.bagg.l$predict(v01.tree.bagg.l, v01.test),
                                v01.test$Class),
                  nb=err(naiveBayes.l$predict(v01.nb.l, v01.test), v01.test$Class))

  # confusion matrix
confmat(rpart.l$predict(v.tree.l, v.test), v.test$Class)
confmat(rpart.bagg.l$predict(v.tree.bagg.l, v.test), v.test$Class)
confmat(naiveBayes.l$predict(v.nb.l, v.test), v.test$Class)

confmat(rpart.l$predict(v01.tree.l, v01.test), v01.test$Class)
confmat(naiveBayes.l$predict(v01.nb.l, v01.test), v01.test$Class)
