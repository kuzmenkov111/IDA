v.tree.err <- c(direct=err(v.tree.pred, v.test$Class),
                nbc=err(v.tree.n.pred, v.test$Class),
                `1ofk`=err(v.tree.1.pred, v.test$Class),
                ecc=err(v.tree.e.pred, v.test$Class))

v.nb.err <- c(direct=err(v.nb.pred, v.test$Class),
                nbc=err(v.nb.n.pred, v.test$Class),
                `1ofk`=err(v.nb.1.pred, v.test$Class),
                ecc=err(v.nb.e.pred, v.test$Class))

g.tree.err <- c(direct=err(g.tree.pred, g.test$Type),
                nbc=err(g.tree.n.pred, g.test$Type),
                `1ofk`=err(g.tree.1.pred, g.test$Type),
                ecc=err(g.tree.e.pred, g.test$Type))

g.nb.err <- c(direct=err(g.nb.pred, g.test$Type),
              nbc=err(g.nb.n.pred, g.test$Type),
              `1ofk`=err(g.nb.1.pred, g.test$Type),
              ecc=err(g.nb.e.pred, g.test$Type))

barplot(v.tree.err, main="Vehicle Silhouettes, rpart", ylab="Error", las=2)
lines(c(0, 5), rep(v.tree.err[1], 2), lty=2)

barplot(v.nb.err, main="Vehicle Silhouettes, naiveBayes", ylab="Error", las=2)
lines(c(0, 5), rep(v.nb.err[1], 2), lty=2)

barplot(g.tree.err, main="Glass, rpart", ylab="Error", las=2)
lines(c(0, 5), rep(g.tree.err[1], 2), lty=2)

barplot(g.nb.err, main="Glass, naiveBayes", ylab="Error", las=2)
lines(c(0, 5), rep(g.nb.err[1], 2), lty=2)
