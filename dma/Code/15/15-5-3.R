## AdaBoost ensemble modeling using up to m base models created using algorithm alg
## with arguments arg and maximum allowed base model error 0.5-eps
adaboost <- function(formula, data, m, alg, eps=0.01, args=NULL, predf=predict)
{
  class <- y.var(formula)
  nc <- nlevels(data[[class]])
  model.weights <- NULL

  abst.reweight <- function(weights, pred)
  {
    e <- werr(pred, data[[class]], weights)
    if (e<=0.5-eps && is.finite(mw <- 0.5*(log((1-e)/e)+log(nc-1))))
    {
      model.weights <<- c(model.weights, mw)
      weights*exp(mw*(2*(pred!=data[[class]])-1))
    }
    else
      NULL
  }

  `class<-`(list(models=base.ensemble.weight.x(formula, data, m, alg, args,
                                               weights=rep(1, nrow(data)),
                                               abst.reweight,
                                               predf=predf),
                 model.weights=model.weights), "adaboost")
}

## AdaBoost prediction
predict.adaboost <- function(boost, data, predf=predict)
{
  predict.ensemble.weighted(boost$models, boost$model.weights, data, predf)
}

  # AdaBoost for the HouseVotes84 data
hv.abst.tree1 <- adaboost(Class~., hv.train, 50, rpart,
                          args=list(minsplit=2, cp=0, maxdepth=1),
                          predf=function(...) predict(..., type="c"))
hv.abst.tree3 <- adaboost(Class~., hv.train, 50, rpart,
                          args=list(minsplit=2, cp=0, maxdepth=3),
                          predf=function(...) predict(..., type="c"))
hv.abst.tree5 <- adaboost(Class~., hv.train, 50, rpart,
                          args=list(minsplit=2, cp=0, maxdepth=5),
                          predf=function(...) predict(..., type="c"))

hv.pred.abst.tree1 <- predict(hv.abst.tree1, hv.test,
                              predf=function(...) predict(..., type="c"))
hv.pred.abst.tree3 <- predict(hv.abst.tree3, hv.test,
                              predf=function(...) predict(..., type="c"))
hv.pred.abst.tree5 <- predict(hv.abst.tree5, hv.test,
                              predf=function(...) predict(..., type="c"))

  # AdaBoost test set errors for the HouseVotes84 data
hv.err.abst <- list(tree1 = err(hv.pred.abst.tree1, hv.test$Class),
                    tree3 = err(hv.pred.abst.tree3, hv.test$Class),
                    tree5 = err(hv.pred.abst.tree5, hv.test$Class))
