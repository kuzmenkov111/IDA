## generate base models by attribute sampling
base.ensemble.sample.a <- function(formula, data, m, alg, args=NULL,
                                   frac=0, replace=TRUE)
{
  attributes <- x.vars(formula, data)
  target <- y.var(formula)
  ns <- ifelse(clip.val(frac, 0, 1)>0, ceiling(frac*length(attributes)),
                                       ceiling(sqrt(length(attributes))))
  lapply(1:m, function(i)
              {
                sa <- sample(length(attributes), ns)
                do.call(alg, c(list(make.formula(target, attributes[sa]), data),
                               args))
              })
}


if (FALSE)
{

  # base models for the HouseVotes84 data
hv.bm.tree.sa <- base.ensemble.sample.a(Class~., hv.train, 50, rpart,
                                        args=list(minsplit=2, cp=0))
hv.bm.nb.sa <- base.ensemble.sample.a(Class~., hv.train, 50, naiveBayes)

  # base models for the BostonHousing data
bh.bm.tree.sa <- base.ensemble.sample.a(medv~., bh.train, 50, rpart,
                                        args=list(minsplit=2, cp=0))
bh.bm.lm.sa <- base.ensemble.sample.a(medv~., bh.train, 50, lm)

  # base model training set errors for the HouseVotes84 data
hv.train.err.tree.sa <- sapply(hv.bm.tree.sa,
                               function(h) err(predict(h, hv.train, type="c"),
                                               hv.train$Class))
hv.train.err.nb.sa <- sapply(hv.bm.nb.sa,
                             function(h) err(predict(h, hv.train), hv.train$Class))

  # base model training set MSE values for the BostonHousing data
bh.train.mse.tree.sa <- sapply(bh.bm.tree.sa,
                               function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.lm.sa <- sapply(bh.bm.lm.sa,
                             function(h) mse(predict(h, bh.train), bh.train$medv))

  # base model test set errors for the HouseVotes84 data
hv.test.err.tree.sa <- sapply(hv.bm.tree.sa,
                              function(h) err(predict(h, hv.test, type="c"),
                                              hv.test$Class))
hv.test.err.nb.sa <- sapply(hv.bm.nb.sa,
                            function(h) err(predict(h, hv.test), hv.test$Class))

  # base model test set MSE values for the BostonHousing data
bh.test.mse.tree.sa <- sapply(bh.bm.tree.sa,
                              function(h) mse(predict(h, bh.test), bh.test$medv))
bh.test.mse.lm.sa <- sapply(bh.bm.lm.sa,
                            function(h) mse(predict(h, bh.test), bh.test$medv))

}
