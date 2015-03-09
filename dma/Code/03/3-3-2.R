class.distribution <- function(n)
{
  tree$count[tree$node==n] <<- sum(nodemap==n)
  tree[tree$node==n,cprobs] <<- pdisc(data[nodemap==n,class])
}

class.distribution(n)
