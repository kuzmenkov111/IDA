## create a piecewise linear model represented by an rpart regression tree
## with linear models corresponding to leaves
lmrpart <- function(formula, data, skip.attr=FALSE, ...)
{
  m.tree <- rpart(formula, data, ...)
  m.leaves <- sort(unique(predict(m.tree, data)))
  lmattr <- if (skip.attr)
              setdiff(x.vars(formula, data), setdiff(m.tree$frame$var, "<leaf>"))
            else "."
  m.lm <- `names<-`(lapply(m.leaves, function(l)
                                     lm(make.formula(y.var(formula), lmattr),
                                        data[predict(m.tree, data)==l,])),
                    m.leaves)
  `class<-`(list(tree=m.tree, lm=m.lm), "lmrpart")
}

## prediction method for lmrpart
predict.lmrpart <- function(model, data)
{
  leaves <- as.character(predict(model$tree, data))
  sapply(1:nrow(data),
         function(i) predict(model$lm[[leaves[i]]], data[i,]))
}


if (FALSE)
{

library(dmr.regeval)

data(BostonHousing, package="mlbench")
BostonHousing$chas <- NULL

set.seed(12)
rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh>=0.33,]
bh.test <- BostonHousing[rbh<0.33,]

bh.mtree <- lmrpart(medv~., bh.train, cp=0.05)

  # generate and evaluate test set predictions
mse(predict(bh.mtree, bh.test), bh.test$medv)
  # compare to the internal regression tree
mse(predict(bh.mtree$tree, bh.test), bh.test$medv)
  # and a single linear model
mse(predict(lm(medv~., bh.train), bh.test), bh.test$medv)

}
