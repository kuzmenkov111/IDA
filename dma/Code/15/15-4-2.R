## combine base models by probability averaging
predict.ensemble.prob <- function(models, data, predf=predict,
                                  prob=FALSE, labels=NULL)
{
  bp <- lapply(models, function(h) predf(h, data))
  cp <- 0
  for (i in (1:(m <- length(bp))))
    cp <- cp + bp[[i]]
  if (prob)
    cp/m
  else
  {
    if (is.null(labels))
      labels <- colnames(cp)
    factor(apply(cp, 1, which.max), levels=1:2, labels=labels)
  }
}

  # combine base models for the HouseVotes84 data
hv.pred.tree.sx.p <- predict.ensemble.prob(hv.bm.tree.sx, hv.test)
hv.pred.nb.sx.p <- predict.ensemble.prob(hv.bm.nb.sx, hv.test,
                                         predf=function(...) predict(..., type="r"))
hv.pred.tree.wx.p <- predict.ensemble.prob(hv.bm.tree.wx, hv.test)
hv.pred.tree.sa.p <- predict.ensemble.prob(hv.bm.tree.sa, hv.test)
hv.pred.nb.sa.p <- predict.ensemble.prob(hv.bm.nb.sa, hv.test,
                                         predf=function(...) predict(..., type="r"))

  # ensemble model test set errors for the HouseVotes84 data
hv.err.p <- c(tree = hv.err.tree,
              tree.sx = err(hv.pred.tree.sx.p, hv.test$Class),
              tree.wx = err(hv.pred.tree.wx.p, hv.test$Class),
              tree.sa = err(hv.pred.tree.sa.p, hv.test$Class),
              nb = hv.err.nb,
              nb.sx = err(hv.pred.nb.sx.p, hv.test$Class),
              nb.sa = err(hv.pred.nb.sa.p, hv.test$Class))

barplot(hv.err.p, main="HouseVotes84", ylab="Error", las=2)
lines(c(0, 9), rep(hv.err.p[1], 2), lty=2)
lines(c(0, 9), rep(hv.err.p[5], 2), lty=3)
