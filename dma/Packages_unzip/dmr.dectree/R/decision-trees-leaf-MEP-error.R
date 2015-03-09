## calculate the MEP error of a given node, if treated as a leaf
leaf.mep.error <- function(rp, node, data, class, m)
{
  e <- leaf.error(rp, node, data, class)
  n <- node.card(rp, node, data)
  nc <- (1-e)*n
  p <- as.double(pdisc(class)[rp$frame$yval[row.names(rp$frame)==node]])
  1-(nc+m*p)/(n+m)
}


if (FALSE)
{

  # MEP error of node 1, if treated as a leaf, for m=0, 2, 5
leaf.mep.error(rptree, 1, weather, weather$play, m=0)
leaf.mep.error(rptree, 1, weather, weather$play, m=2)
leaf.mep.error(rptree, 1, weather, weather$play, m=5)
  # MEP error of node 3, which is actually a leaf, for m=0, 2, 5
leaf.mep.error(rptree, 3, weather, weather$play, m=0)
leaf.mep.error(rptree, 3, weather, weather$play, m=2)
leaf.mep.error(rptree, 1, weather, weather$play, m=5)
  # MEP error of node 4, if treated as a leaf, for m=0, 2, 5
leaf.mep.error(rptree, 4, weather, weather$play, m=0)
leaf.mep.error(rptree, 4, weather, weather$play, m=2)
leaf.mep.error(rptree, 4, weather, weather$play, m=5)

}
