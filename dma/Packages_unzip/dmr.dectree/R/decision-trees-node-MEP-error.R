## calculate the MEP error of the subtree rooted at a given node
node.mep.error <- function(rp, node, data, class, m)
{
  if (rp.leaf(rp, node))
    leaf.mep.error(rp, node, data, class, m)
  else
  {
    el <- node.mep.error(rp, 2*node, data, class, m)
    nl <- node.card(rp, 2*node, data)
    er <- node.mep.error(rp, 2*node+1, data, class, m)
    nr <- node.card(rp, 2*node+1, data)
    weighted.mean(c(el, er), c(nl, nr))
  }
}


if (FALSE)
{

  # MEP error of node 1 for m=0, 2, 5
node.mep.error(rptree, 1, weather, weather$play, m=0)
node.mep.error(rptree, 1, weather, weather$play, m=2)
node.mep.error(rptree, 1, weather, weather$play, m=5)
  # MEP error of node 3, which is actually a leaf, for m=0, 2, 5
node.mep.error(rptree, 3, weather, weather$play, m=0)
node.mep.error(rptree, 3, weather, weather$play, m=2)
node.mep.error(rptree, 3, weather, weather$play, m=5)
  # MEP error of node 4 for m=0, 2, 5
node.mep.error(rptree, 4, weather, weather$play, m=0)
node.mep.error(rptree, 4, weather, weather$play, m=2)
node.mep.error(rptree, 4, weather, weather$play, m=5)

}
