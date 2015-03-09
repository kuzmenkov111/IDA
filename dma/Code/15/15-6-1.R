hv.err <- c(tree=hv.err.tree,
            bagg.tree=hv.err.bagg$tree,
            abst.tree1=hv.err.abst$tree1,
            abst.tree3=hv.err.abst$tree3,
            abst.tree5=hv.err.abst$tree5,
            rf.tree3=hv.err.rf$tree3,
            rf.tree5=hv.err.rf$tree5,
            rf.tree8=hv.err.rf$tree8,
            nb=hv.err.nb,
            bagg.nb=hv.err.bagg$nb,
            rnb=hv.err.rnb$nb)

bh.mse <- c(tree=bh.mse.tree,
            bagg.tree=bh.mse.bagg$tree,
            gbst.tree1=bh.mse.gbst$tree1,
            gbst.tree3=bh.mse.gbst$tree3,
            gbst.tree5=bh.mse.gbst$tree5,
            rf.tree3=bh.mse.rf$tree3,
            rf.tree5=bh.mse.rf$tree5,
            rf.tree8=bh.mse.rf$tree8,
            lm=bh.mse.lm,
            bagg.lm=bh.mse.bagg$lm,
            gbst.lm=bh.mse.gbst$lm)

barplot(hv.err, main="HouseVotes84", ylab="Error", las=2)
lines(c(0, 13), rep(hv.err[1], 2), lty=2)
lines(c(0, 13), rep(hv.err[9], 2), lty=3)

barplot(bh.mse, main="Boston Housing", ylab="MSE", las=2)
lines(c(0, 13), rep(bh.mse[1], 2), lty=2)
lines(c(0, 13), rep(bh.mse[9], 2), lty=3)
