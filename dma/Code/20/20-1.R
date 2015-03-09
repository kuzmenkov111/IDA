## complexity parameter corresponding to the minimum cross-validated error
cpmin <- function(cptab) { cptab[which.min(cptab[,4])[1], 1] }

## complexity parameter corresponding to the smallest tree within 1-SD
## from the minimum cross-validated error
cp1sd <- function(cptab)
{ cptab[which(cptab[,4]<min(cptab[,4]) + cptab[which.min(cptab[,4]),5])[1], 1] }

## sequence of complexity parameter values corresponding to the minimum
## cross-validated error, s next smaller trees, and l next larger trees
cpminrange <- function(cptab, s=5, l=5)
{
  m <- which.min(cptab[,4])[1]
  cptab[max(m-s, 1):min(m+l, nrow(cptab)), 1]
}

## grow and prune an rpart tree using minimum-error cost-complexity pruning
rpart.pmin <- function(formula, data, ...)
{
  tree.f <- rpart(formula, data, minsplit=2, cp=0, ...)
  prune(tree.f, cpmin(tree.f$cptable))
}

## grow and prune an rpart tree using 1-SD cost-complexity pruning
rpart.p1sd <- function(formula, data, ...)
{
  tree.f <- rpart(formula, data, minsplit=2, cp=0, ...)
  prune(tree.f, cp1sd(tree.f$cptable))
}

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
