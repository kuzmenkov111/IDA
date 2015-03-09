data <- weather
attributes <- names(weather)[1:4]
class <- names(weather)[5]

init <- function()
{
  clabs <<- factor(levels(data[[class]]),
                   levels=levels(data[[class]]))      # class labels
  tree <<- data.frame(node=1, attribute=NA, value=NA, class=NA, count=NA,
                      `names<-`(rep(list(NA), length(clabs)),
                                paste("p", clabs, sep=".")))
  cprobs <<- (ncol(tree)-length(clabs)+1):ncol(tree)  # class probability columns
  nodemap <<- rep(1, nrow(data))
  n <<- 1
}

init()
