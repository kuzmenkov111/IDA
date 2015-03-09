## combine base models by using as attributes
## create a model using the specified algorithm and training data
combine.ensemble.attributes <- function(models, data, alg, args=NULL, predf=predict,
                                        append=FALSE)
{
  target <- as.character(models[[1]]$terms[[2]])
  tind <- match(target, names(data))
  data.base <- `names<-`(cbind(data.frame(lapply(models,
                                                 function(h) predf(h, data))),
                               data[[target]]),
                         c(paste("h", 1:length(models), sep=""), target))
  if (append)
    data.base <- cbind(data[,-tind], data.base)
  do.call(alg, c(list(make.formula(target, "."), data.base), args))
}

## combine base models by using as attributes
## predict using the specified base models and combined model
predict.ensemble.attributes <- function(combined.model, base.models, data,
                                        combined.predf=predict, base.predf=predict)
{
  data.pred <- `names<-`(data.frame(lapply(base.models,
                                           function(h) base.predf(h, data))),
                         paste("h", 1:length(base.models), sep=""))
  data.pred <- cbind(data, data.pred)  # make the original attributes available
  combined.predf(combined.model, data.pred)
}

  # combine base models for the HouseVotes84 data
hv.tree.sx.nb <- combine.ensemble.attributes(hv.bm.tree.sx, hv.train, naiveBayes,
                                             predf=function(...)
                                                   predict(..., type="c"))
hv.pred.tree.sx.nb <- predict.ensemble.attributes(hv.tree.sx.nb, hv.bm.tree.sx,
                                                  hv.test,
                                                  base.predf=function(...)
                                                             predict(..., type="c"))
hv.tree.wx.nb <- combine.ensemble.attributes(hv.bm.tree.wx, hv.train, naiveBayes,
                                             predf=function(...)
                                                   predict(..., type="c"))
hv.pred.tree.wx.nb <- predict.ensemble.attributes(hv.tree.wx.nb, hv.bm.tree.wx,
                                                  hv.test,
                                                  base.predf=function(...)
                                                             predict(..., type="c"))
hv.tree.sa.nb <- combine.ensemble.attributes(hv.bm.tree.sa, hv.train, naiveBayes,
                                             predf=function(...)
                                                   predict(..., type="c"))
hv.pred.tree.sa.nb <- predict.ensemble.attributes(hv.tree.sa.nb, hv.bm.tree.sa,
                                                  hv.test,
                                                  base.predf=function(...)
                                                             predict(..., type="c"))

  # combine base models for the BostonHousing data
bh.tree.sx.lm <- combine.ensemble.attributes(bh.bm.tree.sx, bh.train, lm)
bh.pred.tree.sx.lm <- predict.ensemble.attributes(bh.tree.sx.lm, bh.bm.tree.sx,
                                                  bh.test)
bh.tree.wx.lm <- combine.ensemble.attributes(bh.bm.tree.wx, bh.train, lm)
bh.pred.tree.wx.lm <- predict.ensemble.attributes(bh.tree.wx.lm, bh.bm.tree.wx,
                                                  bh.test)
bh.tree.sa.lm <- combine.ensemble.attributes(bh.bm.tree.sa, bh.train, lm)
bh.pred.tree.sa.lm <- predict.ensemble.attributes(bh.tree.sa.lm, bh.bm.tree.sa,
                                                  bh.test)

  # ensemble model test set errors for the HouseVotes84 data
hv.err.a <- c(tree = hv.err.tree,
              tree.sx.nb = err(hv.pred.tree.sx.nb, hv.test$Class),
              tree.wx.nb = err(hv.pred.tree.wx.nb, hv.test$Class),
              tree.sa.nb = err(hv.pred.tree.sa.nb, hv.test$Class))

  # ensemble model test set MSE values for the BostonHousing data
bh.mse.a <- c(tree = bh.mse.tree,
              tree.sx.lm = mse(bh.pred.tree.sx.lm, bh.test$medv),
              tree.wx.lm = mse(bh.pred.tree.wx.lm, bh.test$medv),
              tree.sa.lm = mse(bh.pred.tree.sa.lm, bh.test$medv))

barplot(hv.err.a, main="HouseVotes84", ylab="Error", las=2)
lines(c(0, 5), rep(hv.err.a[1], 2), lty=2)

barplot(bh.mse.a, main="Boston Housing", ylab="MSE", las=2)
lines(c(0, 5), rep(bh.mse.a[1], 2), lty=2)
