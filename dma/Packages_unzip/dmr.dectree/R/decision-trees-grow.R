## a simple decision tree growing implementation
grow.dectree <- function(formula, data,
                         imp=entropy.p, maxprob=0.999, minsplit=2, maxdepth=8)
{
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

  next.node <- function(n)
  {
    if (any(opn <- tree$node>n))
      min(tree$node[opn])
    else Inf
  }

  class.distribution <- function(n)
  {
    tree$count[tree$node==n] <<- sum(nodemap==n)
    tree[tree$node==n,cprobs] <<- pdisc(data[nodemap==n,class])
  }

  class.label <- function(n)
  {
    tree$class[tree$node==n] <<- which.max(tree[tree$node==n,cprobs])
  }

  stop.criteria <- function(n)
  {
    n>=2^maxdepth || tree$count[tree$node==n]<minsplit ||
      max(tree[tree$node==n,cprobs])>maxprob
  }

  split.eval <- function(av, sv, cl)
  {
    cond <- !is.na(av) & (if (is.numeric(av)) av<=as.numeric(sv) else av==sv)
    pd1 <- pdisc(cl[cond])
    n1 <- sum(cond)
    pd0 <- pdisc(cl[!cond])
    n0 <- sum(!cond)

    if (n1>0 && n0>0)
      weighted.impurity(pd1, n1, pd0, n0, imp)
    else
      Inf
  }

  split.select <- function(n)
  {
    splits <- data.frame()
    for (attribute in attributes)
    {
      uav <- sort(unique(data[nodemap==n,attribute]))
      if (length(uav)>1)
        splits <- rbind(splits,
                        data.frame(attribute=attribute,
                                   value=if (is.numeric(uav))
                                           midbrk(uav)
                                         else
                                           as.character(uav),
                                   stringsAsFactors=FALSE))
    }

    if (nrow(splits)>0)
      splits$eval <- sapply(1:nrow(splits),
                            function(s)
                            split.eval(data[nodemap==n,splits$attribute[s]],
                                       splits$value[s],
                                       data[nodemap==n,class]))
    if ((best.eval <- min(splits$eval))<Inf)
      tree[tree$node==n,2:3] <<- splits[which.min(splits$eval),1:2]
    best.eval
  }

  split.apply <- function(n)
  {
    tree <<- rbind(tree,
                   data.frame(node=(2*n):(2*n+1),
                              attribute=NA, value=NA, class=NA, count=NA,
                              `names<-`(rep(list(NA), length(clabs)),
                                        paste("p", clabs, sep="."))))

    av <- data[[tree$attribute[tree$node==n]]]
    cond <- !is.na(av) & (if (is.numeric(av))
                            av<=as.numeric(tree$value[tree$node==n])
                          else av==tree$value[tree$node==n])
    nodemap[nodemap==n & cond] <<- 2*n
    nodemap[nodemap==n & !cond] <<- 2*n+1
  }

  tree <- nodemap <- n <- NULL
  clabs <- cprobs <- NULL
  class <- y.var(formula)
  attributes <- x.vars(formula, data)

  init()
  while (is.finite(n))
  {
    class.distribution(n)
    class.label(n)
    if (!stop.criteria(n))
      if (split.select(n)<Inf)
        split.apply(n)
    n <- next.node(n)
  }
  tree$class <- clabs[tree$class]
  `class<-`(tree, "dectree")
}


## convert a dectree object to a data frame
as.data.frame.dectree <- function(x, row.names=NULL, optional=FALSE, ...)
{ as.data.frame(unclass(x), row.names=row.names, optional=optional) }


if (FALSE)
{

  # grow a decision tree for the weather data
tree <- grow.dectree(play~., weather)

  # grow a decision tree for the weatherc data
treec <- grow.dectree(play~., weatherc)

  # data frame conversion
as.data.frame(tree)
as.data.frame(treec)

}
