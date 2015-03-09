## randomized decision tree growing
## with split selection using ns randomly chosen attributes at each node
## (if unspecified or 0, it defaults to the square root of the number of attributes)
grow.randdectree <- function(formula, data, ns=0,
                             imp=entropy.p, maxprob=0.999, minsplit=2, maxdepth=8)
{
  init <- function()
  {
    clabs <<- factor(levels(data[[class]]),
                     levels=levels(data[[class]]))  # class labels
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
    tree[tree$node==n,"count"] <<- sum(nodemap==n)
    tree[tree$node==n,cprobs] <<- pdisc(data[nodemap==n,class])
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
    for (attribute in sample(attributes, ns))
    {
      uav <- sort(unique(data[nodemap==n,attribute]))
      if (length(uav)>1)
        splits <- rbind(splits,
                        data.frame(attribute=attribute,
                                   value=if (is.numeric(uav))
                                           midbrk(uav)
                                         else as.character(uav),
                                   stringsAsFactors=F))
    }

    if (nrow(splits)>0)
      splits$eval <- sapply(1:nrow(splits),
                            function(s)
                            split.eval(data[nodemap==n,splits$attribute[s]],
                                       splits$value[s],
                                       data[nodemap==n,class]))
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
  ns <- ifelse(ns==0, round(sqrt(length(attributes))),
                      clip.val(ns, 1, length(attributes)))

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


## randomized regression tree growing
## with split selection using ns randomly chosen attributes at each node
## (if unspecified or 0, it defaults to the square root of the number of attributes)
grow.randregtree <- function(formula, data, ns=0,
                             minvar=0.005, minsplit=2, maxdepth=8)
{
  init <- function()
  {
    tree <<- data.frame(node=1, attribute=NA, value=NA, target=NA,
                        count=NA, mean=NA, variance=NA)
    nodemap <<- rep(1, nrow(data))
    n <<- 1
  }

  next.node <- function(n)
  {
    if (any(opn <- tree$node>n))
      min(tree$node[opn])
    else Inf
  }

  target.summary <- function(n)
  {
    tree$count[tree$node==n] <<- sum(nodemap==n)
    tree$mean[tree$node==n] <<- mean(data[nodemap==n,target])
    tree$variance[tree$node==n] <<- var1(data[nodemap==n,target])
  }

  target.value <- function(n)
  {
    tree$target[tree$node==n] <<- tree$mean[tree$node==n]
  }

  stop.criteria <- function(n)
  {
    n>=2^maxdepth || tree$count[tree$node==n]<minsplit ||
      tree$variance[tree$node==n]<minvar
  }

  split.eval <- function(av, sv, tv)
  {
    cond <- !is.na(av) & (if (is.numeric(av)) av<=as.numeric(sv) else av==sv)
    v1 <- tv[cond]
    n1 <- sum(cond)
    v0 <- tv[!cond]
    n0 <- sum(!cond)
    if (n1>0 && n0>0)
      weighted.dispersion(v1, v0)
    else
      Inf
  }

  split.select <- function(n)
  {
    splits <- data.frame()
    for (attribute in sample(attributes, ns))
    {
      uav <- sort(unique(data[nodemap==n,attribute]))
      if (length(uav)>1)
        splits <- rbind(splits,
                        data.frame(attribute=attribute,
                                   value=if (is.numeric(uav))
                                           midbrk(uav)
                                         else as.character(uav),
                                   stringsAsFactors=F))
    }

    if (nrow(splits)>0)
      splits$eval <- sapply(1:nrow(splits),
                            function(s)
                            split.eval(data[nodemap==n,splits$attribute[s]],
                                       splits$value[s],
                                       data[nodemap==n,target]))
    if ((best.eval <- min(splits$eval))<Inf)
      tree[tree$node==n,2:3] <<- splits[which.min(splits$eval),1:2]
    best.eval
  }

  split.apply <- function(n)
  {
    tree <<- rbind(tree,
                   data.frame(node=(2*n):(2*n+1), attribute=NA, value=NA, target=NA,
                              count=NA, mean=NA, variance=NA))

    av <- data[[tree$attribute[tree$node==n]]]
    cond <- !is.na(av) & (if (is.numeric(av))
                            av<=as.numeric(tree$value[tree$node==n])
                          else av==tree$value[tree$node==n])
    nodemap[nodemap==n & cond] <<- 2*n
    nodemap[nodemap==n & !cond] <<- 2*n+1
  }

  tree <- nodemap <- n <- NULL
  target <- y.var(formula)
  attributes <- x.vars(formula, data)
  ns <- ifelse(ns==0, round(length(attributes)/3),
               clip.val(ns, 1, length(attributes)))

  init()
  while (is.finite(n))
  {
    target.summary(n)
    target.value(n)
    if (!stop.criteria(n))
      if (split.select(n)<Inf)
        split.apply(n)
    n <- next.node(n)
  }
  `class<-`(tree, "regtree")
}


## generate base models by simple multiple algorithm application
base.ensemble.simple <- function(formula, data, m, alg, args=NULL)
{
  lapply(1:m, function(i) do.call(alg, c(list(formula, data), args)))
}

  # base models for the HouseVotes84 data
hv.bm.tree.rnd <- base.ensemble.simple(Class~., hv.train, 50, grow.randdectree)

  # base models for the BostonHousing data
bh.bm.tree.rnd <- base.ensemble.simple(medv~., bh.train, 50, grow.randregtree,
                                       args=list(minvar=5))

  # base model training set errors for the HouseVotes84 data
hv.train.err.tree.rnd <- sapply(hv.bm.tree.rnd,
                                function(h) err(predict(h, hv.train),
                                                hv.train$Class))

  # base model training set MSE values for the BostonHousing data
bh.train.mse.tree.rnd <- sapply(bh.bm.tree.rnd,
                                function(h) mse(predict(h, bh.train), bh.train$medv))

  # base model test set errors for the HouseVotes84 data
hv.test.err.tree.rnd <- sapply(hv.bm.tree.rnd,
                               function(h) err(predict(h, hv.test), hv.test$Class))

  # base model test set MSE values for the BostonHousing data
bh.test.mse.tree.rnd <- sapply(bh.bm.tree.rnd,
                               function(h) mse(predict(h, bh.test), bh.test$medv))
