## bagging ensemble modeling using m base models created with algorithm alg
## with arguments arg
bagging <- function(formula, data, m, alg, args=NULL)
{
  `class<-`(base.ensemble.sample.x(formula, data, m, alg, args), "bagging")
}


## bagging prediction
predict.bagging <- function(models, data, predf=predict)
{
  predict.ensemble.basic(models, data, predf)
}


if (FALSE)
{

  # bagging for the HouseVotes84 data
hv.bagg.tree <- bagging(Class~., hv.train, 50, rpart, args=list(minsplit=2, cp=0))
hv.bagg.nb <- bagging(Class~., hv.train, 50, naiveBayes)

hv.pred.bagg.tree <- predict(hv.bagg.tree, hv.test,
                             predf=function(...) predict(..., type="c"))
hv.pred.bagg.nb <- predict(hv.bagg.nb, hv.test)

  # bagging for the BostonHousing data
bh.bagg.tree <- bagging(medv~., bh.train, 50, rpart, args=list(minsplit=2, cp=0))
bh.bagg.lm <- bagging(medv~., bh.train, 50, lm)

bh.pred.bagg.tree <- predict(bh.bagg.tree, bh.test)
bh.pred.bagg.lm <- predict(bh.bagg.lm, bh.test)

  # bagging test set errors for the HouseVotes84 data
hv.err.bagg <- list(tree = err(hv.pred.bagg.tree, hv.test$Class),
                    nb = err(hv.pred.bagg.nb, hv.test$Class))

  # bagging test set MSE values for the Boston Housing data
bh.mse.bagg <- list(tree = mse(bh.pred.bagg.tree, bh.test$medv),
                    lm = mse(bh.pred.bagg.lm, bh.test$medv))

}
