## generate base models by instance sampling
base.ensemble.sample.x <- function(formula, data, m, alg, args=NULL,
                                   size=nrow(data), replace=TRUE)
{
  lapply(1:m, function(i)
              {
                bag <- sample(nrow(data), size=nrow(data), replace=replace)
                do.call(alg, c(list(formula, data[bag,]), args))
              })
}

  # base models for the HouseVotes84 data
hv.bm.tree.sx <- base.ensemble.sample.x(Class~., hv.train, 50, rpart)
hv.bm.nb.sx <- base.ensemble.sample.x(Class~., hv.train, 50, naiveBayes)

  # base models for the BostonHousing data
bh.bm.tree.sx <- base.ensemble.sample.x(medv~., bh.train, 50, rpart)
bh.bm.lm.sx <- base.ensemble.sample.x(medv~., bh.train, 50, lm)

  # base model training set errors for the HouseVotes84 data
hv.train.err.tree.sx <- sapply(hv.bm.tree.sx,
                               function(h) err(predict(h, hv.train, type="c"),
                                               hv.train$Class))
hv.train.err.nb.sx <- sapply(hv.bm.nb.sx,
                             function(h) err(predict(h, hv.train), hv.train$Class))

  # base model training set MSE values for the BostonHousing data
bh.train.mse.tree.sx <- sapply(bh.bm.tree.sx,
                               function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.lm.sx <- sapply(bh.bm.lm.sx,
                             function(h) mse(predict(h, bh.train), bh.train$medv))

  # base model test set errors for the HouseVotes84 data
hv.test.err.tree.sx <- sapply(hv.bm.tree.sx,
                              function(h) err(predict(h, hv.test, type="c"),
                                              hv.test$Class))
hv.test.err.nb.sx <- sapply(hv.bm.nb.sx,
                            function(h) err(predict(h, hv.test), hv.test$Class))

  # base model test set MSE values for the BostonHousing data
bh.test.mse.tree.sx <- sapply(bh.bm.tree.sx,
                              function(h) mse(predict(h, bh.test), bh.test$medv))
bh.test.mse.lm.sx <- sapply(bh.bm.lm.sx,
                            function(h) mse(predict(h, bh.test), bh.test$medv))
