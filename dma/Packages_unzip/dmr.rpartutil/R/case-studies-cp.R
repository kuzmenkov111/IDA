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


if (FALSE)
{

data(Vehicle, package="mlbench")

  # fully grown tree
v.tree.f <- rpart(Class~., Vehicle, minsplit=2, cp=0)

  # minimum-error cost-complexity pruning
v.tree.pmin <- prune(v.tree.f, cpmin(v.tree.f$cptable))
  # 1-SD cost-complexity pruning
v.tree.p1sd <- prune(v.tree.f, cp1sd(v.tree.f$cptable))
  # 8 pruned trees aroung the minimum-error (4 smaller, 3 larger)
v.trees.pmin8 <- lapply(cpminrange(v.tree.f$cptable, s=4, l=3),
                        function(cp) prune(v.tree.f, cp))
}
