## generate base models by simple multiple algorithm application
base.ensemble.simple <- function(formula, data, m, alg, args=NULL)
{
  lapply(1:m, function(i) do.call(alg, c(list(formula, data), args)))
}


if (FALSE)
{

  # base models for the HouseVotes84 data
hv.bm.tree.rnd <- base.ensemble.simple(Class~., hv.train, 50, grow.randdectree)

  # base models for the BostonHousing data
bh.bm.tree.rnd <- base.ensemble.simple(medv~., bh.train, 50, grow.randregtree,
                                       args=list(minvar=5))

  # base model training set errors for the HouseVotes84 data
hv.train.err.tree.rnd <- sapply(hv.bm.tree.rnd,
                                function(h) err(predict(h, hv.train),
                                                hv.train$Class))

  # base model training set MSE values for the BostonHousing data
bh.train.mse.tree.rnd <- sapply(bh.bm.tree.rnd,
                                function(h) mse(predict(h, bh.train), bh.train$medv))

  # base model test set errors for the HouseVotes84 data
hv.test.err.tree.rnd <- sapply(hv.bm.tree.rnd,
                               function(h) err(predict(h, hv.test), hv.test$Class))

  # base model test set MSE values for the BostonHousing data
bh.test.mse.tree.rnd <- sapply(bh.bm.tree.rnd,
                               function(h) mse(predict(h, bh.test), bh.test$medv))

}
