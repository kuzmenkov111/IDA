## generate base models by instance weighting
base.ensemble.weight.x <- function(formula, data, m, alg, args=NULL,
                                   weights=runif(nrow(data), min=0.3, max=3),
                                   reweight=function(w, p=NULL)
                                            runif(nrow(data), min=0.3, max=3),
                                   predf=predict)
{
  skip.cond(lapply(1:m,
                   function(i)
                   {
                     if (!is.null(weights))
                     {
                       h <- do.call(alg, c(list(formula, data, weights=weights),
                                           args))
                       pred <- predf(h, data)
                       if (!is.null(weights <<- reweight(weights, pred)))
                         h
                     }
                   }),
            is.null)
}


if (FALSE)
{

  # base models for the HouseVotes84 data
hv.bm.tree.wx <- base.ensemble.weight.x(Class~., hv.train, 50, rpart)

  # base models for the BostonHousing data
bh.bm.tree.wx <- base.ensemble.weight.x(medv~., bh.train, 50, rpart)
bh.bm.lm.wx <- base.ensemble.weight.x(medv~., bh.train, 50, lm)

  # base model training set errors for the HouseVotes84 data
hv.train.err.tree.wx <- sapply(hv.bm.tree.wx,
                               function(h) err(predict(h, hv.train, type="c"),
                                               hv.train$Class))

  # base model training set MSE values for the BostonHousing data
bh.train.mse.tree.wx <- sapply(bh.bm.tree.wx,
                               function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.lm.wx <- sapply(bh.bm.lm.wx,
                             function(h) mse(predict(h, bh.train), bh.train$medv))

  # base model test set errors for the HouseVotes84 data
hv.test.err.tree.wx <- sapply(hv.bm.tree.wx,
                              function(h) err(predict(h, hv.test, type="c"),
                                              hv.test$Class))

  # base model test set MSE values for the BostonHousing data
bh.test.mse.tree.wx <- sapply(bh.bm.tree.wx,
                              function(h) mse(predict(h, bh.test), bh.test$medv))
bh.test.mse.lm.wx <- sapply(bh.bm.lm.wx,
                            function(h) mse(predict(h, bh.test), bh.test$medv))

}
