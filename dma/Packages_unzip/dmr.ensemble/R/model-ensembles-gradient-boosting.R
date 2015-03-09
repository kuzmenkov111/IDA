## gradient boosting ensemble modeling using up to m base models
## created with algorithm alg with arguments arg
gradboost <- function(formula, data, m, alg, beta=0.1, args=NULL, predf=predict)
{
  attributes <- x.vars(formula, data)
  aind <- match(attributes, names(data))
  f <- y.var(formula)
  find <- match(f, names(data))

  models <- list(do.call(alg, c(list(formula, data), args)))
  model.weights <- 1

  for (i in (2:m))
  {
    res <- data[,find]-predict.gradboost(list(models=models,
                                              model.weights=model.weights),
                                         data, predf=predf)
    data.i <- eval(parse(text=paste("cbind(data[,aind],", f, "=res)")))
    models <- c(models, list(h <- do.call(alg, c(list(formula, data.i), args))))
    model.weights <- c(model.weights,
                       beta*sum(res*(pred <- predf(h, data)))/sum(pred^2))
  }
  `class<-`(list(models=models, model.weights=model.weights), "gradboost")
}

## gradient boosting prediction
predict.gradboost <- function(boost, data, predf=predict)
{
  predict.ensemble.weighted(boost$models, boost$model.weights, data, predf,
                            summing=TRUE)
}


if (FALSE)
{

  # gradient boosting for the BostonHousing data
bh.gbst.tree1 <- gradboost(medv~., bh.train, 50, rpart,
                           args=list(minsplit=2, cp=0, maxdepth=1))
bh.gbst.tree3 <- gradboost(medv~., bh.train, 50, rpart,
                           args=list(minsplit=2, cp=0, maxdepth=3))
bh.gbst.tree5 <- gradboost(medv~., bh.train, 50, rpart,
                           args=list(minsplit=2, cp=0, maxdepth=5))
bh.gbst.lm <- gradboost(medv~., bh.train, 50, lm)

bh.pred.gbst.tree1 <- predict(bh.gbst.tree1, bh.test)
bh.pred.gbst.tree3 <- predict(bh.gbst.tree3, bh.test)
bh.pred.gbst.tree5 <- predict(bh.gbst.tree5, bh.test)
bh.pred.gbst.lm <- predict(bh.gbst.lm, bh.test)

  # gradient boosting test set MSE values for the BostonHousing data
bh.mse.gbst <- list(tree1 = mse(bh.pred.gbst.tree1, bh.test$medv),
                    tree3 = mse(bh.pred.gbst.tree3, bh.test$medv),
                    tree5 = mse(bh.pred.gbst.tree5, bh.test$medv),
                    lm = mse(bh.pred.gbst.lm, bh.test$medv))
}
