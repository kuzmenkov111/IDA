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


if (FALSE)
{

  # error of node 1, if treated as a leaf
leaf.error(rptree, 1, weather, weather$play)
  # error of node 3, which is actually a leaf
leaf.error(rptree, 3, weather, weather$play)
  # error of node 4, if treated as a leaf
leaf.error(rptree, 4, weather, weather$play)

}
