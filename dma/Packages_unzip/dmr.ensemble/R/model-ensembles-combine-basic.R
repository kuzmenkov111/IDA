## combine base models by voting/averaging
predict.ensemble.basic <- function(models, data, predf=predict)
{
  bp <- data.frame(lapply(models, function(h) predf(h, data)))
  combf <- if (is.numeric(bp[,1])) mean else modal  # combination scheme
  cp <- sapply(1:nrow(bp), function(i) combf(as.vector(as.matrix(bp[i,]))))
  if (is.factor(bp[,1]))
    factor(cp, levels=levels(bp[,1]))
  else
    cp
}


if (FALSE)
{

  # combine base models for the HouseVotes84 data
hv.pred.tree.sx.b <- predict.ensemble.basic(hv.bm.tree.sx, hv.test,
                                            predf=function(...)
                                                  predict(..., type="c"))
hv.pred.nb.sx.b <- predict.ensemble.basic(hv.bm.nb.sx, hv.test)
hv.pred.tree.wx.b <- predict.ensemble.basic(hv.bm.tree.wx, hv.test,
                                            predf=function(...)
                                                  predict(..., type="c"))
hv.pred.tree.sa.b <- predict.ensemble.basic(hv.bm.tree.sa, hv.test,
                                            predf=function(...)
                                                  predict(..., type="c"))
hv.pred.nb.sa.b <- predict.ensemble.basic(hv.bm.nb.sa, hv.test)
hv.pred.tree.rnd.b <- predict.ensemble.basic(hv.bm.tree.rnd, hv.test)

  # combine base models for the BostonHousing data
bh.pred.tree.sx.b <- predict.ensemble.basic(bh.bm.tree.sx, bh.test)
bh.pred.lm.sx.b <- predict.ensemble.basic(bh.bm.lm.sx, bh.test)
bh.pred.tree.wx.b <- predict.ensemble.basic(bh.bm.tree.wx, bh.test)
bh.pred.lm.wx.b <- predict.ensemble.basic(bh.bm.lm.wx, bh.test)
bh.pred.tree.sa.b <- predict.ensemble.basic(bh.bm.tree.sa, bh.test)
bh.pred.lm.sa.b <- predict.ensemble.basic(bh.bm.lm.sa, bh.test)
bh.pred.tree.rnd.b <- predict.ensemble.basic(bh.bm.tree.rnd, bh.test)

  # ensemble model test set errors for the HouseVotes84 data
hv.err.b <- c(tree = hv.err.tree,
              tree.sx = err(hv.pred.tree.sx.b, hv.test$Class),
              tree.wx = err(hv.pred.tree.wx.b, hv.test$Class),
              tree.sa = err(hv.pred.tree.sa.b, hv.test$Class),
              tree.rnd = err(hv.pred.tree.rnd.b, hv.test$Class),
              nb = hv.err.nb,
              nb.sx = err(hv.pred.nb.sx.b, hv.test$Class),
              nb.sa = err(hv.pred.nb.sa.b, hv.test$Class))

  # ensemble model test set MSE values for the BostonHousing data
bh.mse.b <- c(tree = bh.mse.tree,
              tree.sx = mse(bh.pred.tree.sx.b, bh.test$medv),
              tree.wx = mse(bh.pred.tree.wx.b, bh.test$medv),
              tree.sa = mse(bh.pred.tree.sa.b, bh.test$medv),
              tree.rnd = mse(bh.pred.tree.rnd.b, bh.test$medv),
              lm = bh.mse.lm,
              lm.sx = mse(bh.pred.lm.sx.b, bh.test$medv),
              lm.wx = mse(bh.pred.lm.wx.b, bh.test$medv),
              lm.sa = mse(bh.pred.lm.sa.b, bh.test$medv))

barplot(hv.err.b, main="HouseVotes84", ylab="Error", las=2, col="blue")
lines(c(0, 10), rep(hv.err.b[1], 2), col="red")
lines(c(0, 10), rep(hv.err.b[6], 2), col="orange")

barplot(bh.mse.b, main="Boston Housing", ylab="MSE", las=2, col="blue")
lines(c(0, 11), rep(bh.mse.b[1], 2), col="red")
lines(c(0, 11), rep(bh.mse.b[6], 2), col="orange")

}
