## transform rpart split conditions to a convenient form
rewrite.splits <- function(cond)
{
  ss <- strsplit(cond, "=")[[1]]
  attribute=ss[1]
  values <- sapply(strsplit(ss[2], ",")[[1]], deparse)
  newcond <- paste(attribute, "==", values, sep="", collapse="|")
  return(paste("(", newcond, ")", sep=""))
}

## extract a rule from an rpart tree corresponding to the path from the root
## to a given node
extract.rule <- function(rp, node)
{
  path <- path.rpart(rp, node, print.it=FALSE)[[1]][-1]
  ifelse(length(path)>0, paste(sapply(path, rewrite.splits), collapse="&"), "TRUE")
}

## calculate the error of a given node, if treated as a leaf
leaf.error <- function(rp, node, data, class)
{
  rule <- extract.rule(rp, node)
  dsub <- eval(parse(text=rule), data)
  lab <- levels(class)[rp$frame$yval[row.names(rp$frame)==node]]
  sum(lab!=class[dsub])/nrow(data[dsub,])
}

  # error of node 1, if treated as a leaf
leaf.error(rptree, 1, weather, weather$play)
  # error of node 3, which is actually a leaf
leaf.error(rptree, 3, weather, weather$play)
  # error of node 4, if treated as a leaf
leaf.error(rptree, 4, weather, weather$play)

## check whether a given node is a leaf of an rpart tree
rp.leaf <- function(rp, node)
{
  rp$frame$var[row.names(rp$frame)==node]=="<leaf>"
}

## calculate the number of instances corresponding to a node
node.card <- function(rp, node, data)
{
  rule <- extract.rule(rp, node)
  dsub <- eval(parse(text=rule), data)
  nrow(data[dsub,])
}

## calculate the error of the subtree rooted at a given node
node.error <- function(rp, node, data, class)
{
  if (rp.leaf(rp, node))
    leaf.error(rp, node, data, class)
  else
  {
    el <- node.error(rp, 2*node, data, class)
    nl <- node.card(rp, 2*node, data)
    er <- node.error(rp, 2*node+1, data, class)
    nr <- node.card(rp, 2*node+1, data)
    weighted.mean(c(el, er), c(nl, nr))
  }
}

  # error of node 1
node.error(rptree, 1, weather, weather$play)
  # error of node 3, which is actually a leaf
node.error(rptree, 3, weather, weather$play)
  # error of node 4
node.error(rptree, 1, weather, weather$play)

  # check which nodes satisfy the REP criterion
sapply(as.integer(row.names(rptree$frame)),
       function(node)
       {
         !rp.leaf(rptree, node) &&
           leaf.error(rptree, node, weather, weather$play)<=
             node.error(rptree, node, weather, weather$play)
       })
