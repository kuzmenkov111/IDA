data <- weatherr
attributes <- names(weatherr)[1:4]
target <- names(weatherr)[5]

init <- function()
{
  tree <<- data.frame(node=1, attribute=NA, value=NA, target=NA,
                      count=NA, mean=NA, variance=NA)
  nodemap <<- rep(1, nrow(data))
  n <<- 1
}

init()
