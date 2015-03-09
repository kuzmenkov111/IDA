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


if (FALSE)
{

data(Vehicle, package="mlbench")

v.tree.pmin <- rpart.pmin(Class~., Vehicle)
v.tree.p1sd <- rpart.p1sd(Class~., Vehicle)

}
