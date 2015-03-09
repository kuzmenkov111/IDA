  # mean misclassification cost with respect to the cost matrix
v.mc <- c(tree=v.mc.b$tree,
          tree.w=v.mc.w$tree,
          tree.s=v.mc.s$tree,
          tree.m=v.mc.m$tree,
          tree.l=v.mc.l$tree,
          tree.bagg.l=v.mc.l$tree.bagg,
          nb=v.mc.b$nb,
          nb.s=v.mc.s$nb,
          nb.m=v.mc.m$nb,
          nb.l=v.mc.l$nb)

v01.mc <- c(tree=v01.mc.b$tree,
            tree.w=v01.mc.w$tree,
            tree.s=v01.mc.s$tree,
            tree.m=v01.mc.m$tree,
            tree.l=v01.mc.l$tree,
            tree.bagg.l=v01.mc.l$tree.bagg,
            nb=v01.mc.b$nb,
            nb.s=v01.mc.s$nb,
            nb.m=v01.mc.m$nb,
            nb.l=v01.mc.l$nb)

  # mean misclassification cost with respect to the per-class cost vector
v.mcc <- c(tree=v.mcc.b$tree,
           tree.w=v.mcc.w$tree,
           tree.s=v.mcc.s$tree,
           tree.m=v.mcc.m$tree,
           tree.l=v.mcc.l$tree,
           tree.bagg.l=v.mcc.l$tree.bagg,
           nb=v.mcc.b$nb,
           nb.s=v.mcc.s$nb,
           nb.m=v.mcc.m$nb,
           nb.l=v.mcc.l$nb)

  # misclassification error
v.err <- c(tree=v.err.b$tree,
           tree.w=v.err.w$tree,
           tree.s=v.err.s$tree,
           tree.m=v.err.m$tree,
           tree.l=v.err.l$tree,
           tree.bagg.l=v.err.l$tree.bagg,
           nb=v.err.b$nb,
           nb.s=v.err.s$nb,
           nb.m=v.err.m$nb,
           nb.l=v.err.l$nb)

v01.err <- c(tree=v01.err.b$tree,
             tree.w=v01.err.w$tree,
             tree.s=v01.err.s$tree,
             tree.m=v01.err.m$tree,
             tree.l=v01.err.l$tree,
             tree.bagg.l=v01.err.l$tree.bagg,
             nb=v01.err.b$nb,
             nb.s=v01.err.s$nb,
             nb.m=v01.err.m$nb,
             nb.l=v01.err.l$nb)


barplot(v.mc, ylab="Mean cost (matrix)", las=2)
lines(c(0, 12), rep(v.mc[1], 2), lty=2)
lines(c(0, 12), rep(v.mc[7], 2), lty=3)

barplot(v.mcc, ylab="Mean cost (per-class)", las=2)
lines(c(0, 12), rep(v.mcc[1], 2), lty=2)
lines(c(0, 12), rep(v.mcc[7], 2), lty=3)

barplot(v.err, ylab="Error", las=2)
lines(c(0, 12), rep(v.err[1], 2), lty=2)
lines(c(0, 12), rep(v.err[7], 2), lty=3)

barplot(v01.mc, ylab="Mean cost (two-class)", las=2)
lines(c(0, 12), rep(v01.mc[1], 2), lty=2)
lines(c(0, 12), rep(v01.mc[7], 2), lty=3)

barplot(v01.err, ylab="Error (two-class)", las=2)
lines(c(0, 12), rep(v01.err[1], 2), lty=2)
lines(c(0, 12), rep(v01.err[7], 2), lty=3)
