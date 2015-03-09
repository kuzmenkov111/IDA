rhom2c <- function(rhom)
{
  apply(rhom, 2, sum)/(nrow(rhom)-1)
}

rhoc2m <- function(rhoc)
{
  rhom <- matrix(rhoc, nrow=length(rhoc), ncol=length(rhoc), byrow=TRUE,
                 dimnames=list(predicted=names(rhoc), true=names(rhoc)))
  `diag<-`(rhom, 0)
}

  # per-class cost vectors
v.rc <- rhom2c(v.rm)
v01.rc <- rhom2c(v01.rm)

  # per-class cost vectors in a matrix representation
v.rcm <- rhoc2m(v.rc)
v01.rcm <- rhoc2m(v01.rc)

  # misclassification cost for cost-insensitive models
  # with respect to the per-class cost vector
v.mcc.b <- list(tree=mean.cost(predict(v.tree, v.test, type="c"),
                               v.test$Class, v.rcm),
                nb=mean.cost(predict(v.nb, v.test), v.test$Class, v.rcm))
