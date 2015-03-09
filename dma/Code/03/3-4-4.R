## calculate the PEP error of the subtree rooted at a given node
node.pep.error <- function(rp, node, data, class)
{
  e <- node.error(rp, node, data, class)
  n <- node.card(rp, node, data)
  e1 <- (e*n+1)/(n+2)
  e + sqrt(e1*(1-e1)/n)
}

  # PEP error of node 1
node.pep.error(rptree, 1, weather, weather$play)
  # PEP error of node 4
node.pep.error(rptree, 1, weather, weather$play)

  # check which nodes would get pruned under the PEP criterion
sapply(as.integer(row.names(rptree$frame)),
       function(node)
       {
         !rp.leaf(rptree, node) &&
           leaf.error(rptree, node, weather, weather$play)<=
             node.pep.error(rptree, node, weather, weather$play)
       })
