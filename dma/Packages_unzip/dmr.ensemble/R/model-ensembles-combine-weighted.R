## combine base models by weighted voting/averaging/summing
predict.ensemble.weighted <- function(models, weights, data, predf=predict,
                                      summing=FALSE)
{
  bp <- data.frame(lapply(models, function(h) predf(h, data)))
  combf <- if (is.numeric(bp[,1])) weighted.mean
           else weighted.modal  # combination scheme
  cp <- sapply(1:nrow(bp), function(i) combf(as.vector(as.matrix(bp[i,])), weights))
  if (is.numeric(bp[,1]) && summing)
    cp <- cp*sum(weights)  # summing instead of averaging requested
  if (is.factor(bp[,1]))
    factor(cp, levels=levels(bp[,1]))
  else
    cp
}


if (FALSE)
{

  # combine base models for the HouseVotes84 data
hv.pred.tree.sx.w <- predict.ensemble.weighted(hv.bm.tree.sx,
                                               1/(hv.train.err.tree.sx+0.01),
                                               hv.test,
                                               predf=function(...)
                                                     predict(..., type="c"))
hv.pred.nb.sx.w <- predict.ensemble.weighted(hv.bm.nb.sx,
                                             1/(hv.train.err.nb.sx+0.01),
                                             hv.test)
hv.pred.tree.wx.w <- predict.ensemble.weighted(hv.bm.tree.wx,
                                               1/(hv.train.err.tree.wx+0.01),
                                               hv.test,
                                               predf=function(...)
                                                     predict(..., type="c"))
hv.pred.tree.sa.w <- predict.ensemble.weighted(hv.bm.tree.sa,
                                               1/(hv.train.err.tree.sa+0.01),
                                               hv.test,
                                               predf=function(...)
                                                     predict(..., type="c"))
hv.pred.nb.sa.w <- predict.ensemble.weighted(hv.bm.nb.sa,
                                             1/(hv.train.err.nb.sa+0.01), hv.test)
hv.pred.tree.rnd.w <- predict.ensemble.weighted(hv.bm.tree.rnd,
                                                1/(hv.train.err.tree.rnd+0.01),
                                                hv.test)

  # combine base models for the BostonHousing data
bh.pred.tree.sx.w <- predict.ensemble.weighted(bh.bm.tree.sx,
                                               1/(bh.train.mse.tree.sx+1), bh.test)
bh.pred.lm.sx.w <- predict.ensemble.weighted(bh.bm.lm.sx,
                                             1/(bh.train.mse.lm.sx+1), bh.test)
bh.pred.tree.wx.w <- predict.ensemble.weighted(bh.bm.tree.wx,
                                               1/(bh.train.mse.tree.wx+1), bh.test)
bh.pred.lm.wx.w <- predict.ensemble.weighted(bh.bm.lm.wx,
                                             1/(bh.train.mse.lm.wx+1), bh.test)
bh.pred.tree.sa.w <- predict.ensemble.weighted(bh.bm.tree.sa,
                                               1/(bh.train.mse.tree.sa+1), bh.test)
bh.pred.lm.sa.w <- predict.ensemble.weighted(bh.bm.lm.sa,
                                             1/(bh.train.mse.lm.sa+1), bh.test)
bh.pred.tree.rnd.w <- predict.ensemble.weighted(bh.bm.tree.rnd,
                                                1/(bh.train.mse.tree.rnd+1), bh.test)

  # ensemble model test set errors for the HouseVotes84 data
hv.err.w <- c(tree = hv.err.tree,
              tree.sx = err(hv.pred.tree.sx.w, hv.test$Class),
              tree.wx = err(hv.pred.tree.wx.w, hv.test$Class),
              tree.sa = err(hv.pred.tree.sa.w, hv.test$Class),
              tree.rnd = err(hv.pred.tree.rnd.w, hv.test$Class),
              nb = hv.err.nb,
              nb.sx = err(hv.pred.nb.sx.w, hv.test$Class),
              nb.sa = err(hv.pred.nb.sa.w, hv.test$Class))

  # ensemble model test set MSE values for the BostonHousing data
bh.mse.w <- c(tree = bh.mse.tree,
              tree.sx = mse(bh.pred.tree.sx.w, bh.test$medv),
              tree.wx = mse(bh.pred.tree.wx.w, bh.test$medv),
              tree.sa = mse(bh.pred.tree.sa.w, bh.test$medv),
              tree.rnd = mse(bh.pred.tree.rnd.w, bh.test$medv),
              lm = bh.mse.lm,
              lm.sx = mse(bh.pred.lm.sx.w, bh.test$medv),
              lm.wx = mse(bh.pred.lm.wx.w, bh.test$medv),
              lm.sa = mse(bh.pred.lm.sa.w, bh.test$medv))

barplot(hv.err.w, main="HouseVotes84", ylab="Error", las=2, col="blue")
lines(c(0, 10), rep(hv.err.w[1], 2), col="red")
lines(c(0, 10), rep(hv.err.w[6], 2), col="orange")

barplot(bh.mse.w, main="Boston Housing", ylab="MSE", las=2, col="blue")
lines(c(0, 11), rep(bh.mse.w[1], 2), col="red")
lines(c(0, 11), rep(bh.mse.w[6], 2), col="orange")

}
