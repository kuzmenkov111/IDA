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
  for (attribute in attributes)
  {
    uav <- sort(unique(data[nodemap==n,attribute]))
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
                          split.eval(data[nodemap==n,splits$attribute[s]],
                                     splits$value[s],
                                     data[nodemap==n,target]))
  if ((best.eval <- min(splits$eval))<Inf)
    tree[tree$node==n,2:3] <<- splits[which.min(splits$eval),1:2]
  best.eval
}

  # variance-based split selection
split.select(n)
