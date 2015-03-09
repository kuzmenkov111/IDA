class.label <- function(n)
{
  tree$class[tree$node==n] <<- which.max(tree[tree$node==n,cprobs])
}

class.label(n)
