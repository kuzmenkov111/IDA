## random forest ensemble modeling using m randomized decision or regression trees
## with ns randomly selected attributes at each node used for splitting
randforest <- function(formula, data, m, ns=0, args=NULL)
{
  target <- y.var(formula)
  alg <- if (!is.numeric(data[[target]])) grow.randdectree else grow.randregtree

  `class<-`(base.ensemble.sample.x(formula, data, m, alg, args=c(list(ns=ns), args)),
            "randforest")
}


## random forest prediction
predict.randforest <- function(rf, data)
{
  predict.ensemble.basic(rf, data)
}


if (FALSE)
{

  # random forest for the HouseVotes84 data
hv.rf.tree3 <- randforest(Class~., hv.train, 50, args=list(maxdepth=3))
hv.rf.tree5 <- randforest(Class~., hv.train, 50, args=list(maxdepth=5))
hv.rf.tree8 <- randforest(Class~., hv.train, 50, args=list(maxdepth=8))

hv.pred.rf.tree3 <- predict(hv.rf.tree3, hv.test)
hv.pred.rf.tree5 <- predict(hv.rf.tree5, hv.test)
hv.pred.rf.tree8 <- predict(hv.rf.tree8, hv.test)

  # random forest for the BostonHousing data
bh.rf.tree3 <- randforest(medv~., bh.train, 50, args=list(maxdepth=3))
bh.rf.tree5 <- randforest(medv~., bh.train, 50, args=list(maxdepth=5))
bh.rf.tree8 <- randforest(medv~., bh.train, 50, args=list(maxdepth=8))

bh.pred.rf.tree3 <- predict(bh.rf.tree3, bh.test)
bh.pred.rf.tree5 <- predict(bh.rf.tree8, bh.test)
bh.pred.rf.tree8 <- predict(bh.rf.tree8, bh.test)

  # random forest test set errors for the HouseVotes84 data
hv.err.rf <- list(tree3 = err(hv.pred.rf.tree3, hv.test$Class),
                  tree5 = err(hv.pred.rf.tree5, hv.test$Class),
                  tree8 = err(hv.pred.rf.tree8, hv.test$Class))

  # random forest test set MSE values for the BostonHousing data
bh.mse.rf <- list(tree3 = mse(bh.pred.rf.tree3, bh.test$medv),
                  tree5 = mse(bh.pred.rf.tree5, bh.test$medv),
                  tree8 = mse(bh.pred.rf.tree8, bh.test$medv))

}
