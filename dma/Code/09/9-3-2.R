target.summary <- function(n)
{
  tree$count[tree$node==n] <<- sum(nodemap==n)
  tree$mean[tree$node==n] <<- mean(data[nodemap==n,target])
  tree$variance[tree$node==n] <<- var1(data[nodemap==n,target])
}

target.summary(n)
