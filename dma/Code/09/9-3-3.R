target.value <- function(n)
{
  tree$target[tree$node==n] <<- tree$mean[tree$node==n]
}

target.value(n)
