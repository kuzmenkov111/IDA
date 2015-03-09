maxprob <- 0.999
minsplit <- 2
maxdepth <- 8

stop.criteria <- function(n)
{
  n>=2^maxdepth || tree$count[tree$node==n]<minsplit ||
    max(tree[tree$node==n,cprobs])>maxprob
}

stop.criteria(n)
