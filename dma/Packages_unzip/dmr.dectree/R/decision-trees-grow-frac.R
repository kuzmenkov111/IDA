## a simple decision tree growing implementation
## with missing value support using fractional instances
grow.dectree.frac <- function(formula, data,
                              imp=entropy.p, maxprob=0.999, minsplit=2, maxdepth=8)
{
  nmn <- function(n) { nodemap[,"node"]==n }            # nodemap entries for node n
  inn <- function(n)
  { nodemap[nodemap[,"node"]==n,"instance"] }                  # instances at node n
  wgn <- function(n) { nodemap[nodemap[,"node"]==n,"weight"] }   # weights at node n

  init <- function()
  {
    clabs <<- factor(levels(data[[class]]),
                     levels=levels(data[[class]]))      # class labels
    tree <<- data.frame(node=1, attribute=NA, value=NA, class=NA, count=NA,
                        `names<-`(rep(list(NA), length(clabs)),
                                  paste("p", clabs, sep=".")))
    cprobs <<- (ncol(tree)-length(clabs)+1):ncol(tree)  # class probability columns
    nodemap <<- cbind(instance=1:nrow(data), node=rep(1, nrow(data)),
                      weight=rep(1, nrow(data)))
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
    tree[tree$node==n,"count"] <<- sum(wgn(n))
    tree[tree$node==n,cprobs] <<- weighted.pdisc(data[inn(n),class], w=wgn(n))
  }

  class.label <- function(n)
  {
    tree$class[tree$node==n] <<- which.max(tree[tree$node==n,cprobs])
  }

  stop.criteria <- function(n)
  {
    n>=2^maxdepth || tree[tree$node==n,"count"]<minsplit ||
      max(tree[tree$node==n,cprobs])>maxprob
  }

  split.eval <- function(av, sv, cl, w)
  {
    cond <- if (is.numeric(av)) av<=as.numeric(sv) else av==sv
    cond1 <- !is.na(av) & cond   # true split outcome
    cond0 <- !is.na(av) & !cond  # false split outcome

    pd1 <- weighted.pdisc(cl[cond1], w=w[cond1])
    n1 <- sum(w[cond1])
    pd0 <- weighted.pdisc(cl[cond0], w=w[cond0])
    n0 <- sum(w[cond0])
    pdm <- weighted.pdisc(cl[is.na(av)], w=w[is.na(av)])
    nm <- sum(w[is.na(av)])

    if (nm>0)
    {
      p1 <- if (n1+n0>0) n1/(n1+n0) else 0.5
      p0 <- 1-p1
      pd1 <- (n1*pd1 + p1*nm*pdm)/(n1+p1*nm)
      n1 <- n1 + p1*nm
      pd0 <- (n0*pd0 + p0*nm*pdm)/(n0+p0*nm)
      n0 <- n0 + nm*p0
    }

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
      uav <- sort(unique(data[inn(n),attribute]))
      if (length(uav)>1)
        splits <- rbind(splits,
                        data.frame(attribute=attribute,
                                   value=if (is.numeric(uav))
                                           midbrk(uav)
                                         else as.character(uav),
                                   stringsAsFactors=FALSE))
    }

    if (nrow(splits)>0)
      splits$eval <- sapply(1:nrow(splits),
                            function(s)
                            split.eval(data[inn(n),splits$attribute[s]],
                                       splits$value[s],
                                       data[inn(n),class], wgn(n)))
    if ((best.eval <- min(splits$eval))<Inf)
      tree[tree$node==n,2:3] <<- splits[which.min(splits$eval),1:2]
    return(best.eval)
  }

  split.apply <- function(n)
  {
    tree <<- rbind(tree,
                   data.frame(node=(2*n):(2*n+1),
                              attribute=NA, value=NA, class=NA, count=NA,
                              `names<-`(rep(list(NA), length(clabs)),
                                        paste("p", clabs, sep="."))))

    av <- data[nodemap[,"instance"],tree$attribute[tree$node==n]]
    cond <- if (is.numeric(av)) av<=as.numeric(tree$value[tree$node==n])
            else av==tree$value[tree$node==n]
    cond1 <- !is.na(av) & cond   # true split outcome
    cond0 <- !is.na(av) & !cond  # false split outcome

    n1 <- sum(nodemap[nmn(n) & cond1,"weight"])
    n0 <- sum(nodemap[nmn(n) & cond0,"weight"])
    nm <- sum(nodemap[nmn(n) & is.na(av),"weight"])

    nodemap[nmn(n) & cond1,"node"] <<- 2*n
    nodemap[nmn(n) & cond0,"node"] <<- 2*n+1

    if (nm>0)
    {
      p1 <- if (n1+n0>0) n1/(n1+n0) else 0.5
      p0 <- 1-p1
      newnn <- nodemap[nmn(n) & is.na(av),,drop=FALSE]
      nodemap[nmn(n) & is.na(av),"weight"] <<-
        p1*nodemap[nmn(n) & is.na(av),"weight"]
      nodemap[nmn(n) & is.na(av),"node"] <<- 2*n
      newnn[,"weight"] <- p0*newnn[,"weight"]
      newnn[,"node"] <- 2*n+1
      nodemap <<- rbind(nodemap, newnn)
    }
  }

  tree <- cprobs <- nodemap <- n <- NULL
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
  `class<-`(tree, "dectree.frac")
}


## convert a dectree.frac object to a data frame
as.data.frame.dectree.frac <- function(x, row.names=NULL, optional=FALSE, ...)
{ as.data.frame(unclass(x), row.names=row.names, optional=optional) }


if (FALSE)
{

  # grow a decision tree for the weather data with missing attribute values
weatherm <- weather
weatherm$outlook[1] <- NA
weatherm$humidity[1:2] <- NA
treem <- grow.dectree.frac(play~., weatherm)

  # grow a decision tree for the weatherc data with missing attribute values
weathercm <- weather
weathercm$temperature[1:2] <- NA
weathercm$humidity[1] <- NA
treecm <- grow.dectree.frac(play~., weathercm)

  # data frame conversion
as.data.frame(treem)
as.data.frame(treecm)

}
