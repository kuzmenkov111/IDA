  # discretization models for the Vehicle Silhouettes data
v.disc <- list(nodisc=`class<-`(list(), "disc"),
               ew=v.disc.ew, ef=v.disc.ef,
               bu.ic1=v.disc.bu.ic1, bu.ic2=v.disc.bu.ic2,
               bu.dc1=v.disc.bu.dc1, bu.dc2=v.disc.bu.dc2, bu.dc3=v.disc.bu.dc3,
               bu.mc=v.disc.bu.mc, bu.e=v.disc.bu.e, bu.chi=v.disc.bu.chi,
               td.mc=v.disc.td.mc, td.e=v.disc.td.e, td.chi=v.disc.td.chi)

  # discretization models for the Glass data
g.disc <- list(nodisc=`class<-`(list(), "disc"),
               ew=g.disc.ew, ef=g.disc.ef,
               bu.ic1=g.disc.bu.ic1, bu.ic2=g.disc.bu.ic2,
               bu.dc1=g.disc.bu.dc1, bu.dc2=g.disc.bu.dc2, bu.dc3=g.disc.bu.dc3,
               bu.mc=g.disc.bu.mc, bu.e=g.disc.bu.e, bu.chi=g.disc.bu.chi,
               td.mc=g.disc.td.mc, td.e=g.disc.td.e, td.chi=g.disc.td.chi)

  # misclassification error values for the Vehicle Silhouettes data
v.err <- lapply(v.disc,
                function(dm)
                {
                  v.train.d <- predict(dm, v.train)
                  v.test.d <- predict(dm, v.test)
                  v.tree.d <- rpart(Class~., v.train.d)
                  v.nb.d <- naiveBayes(Class~., v.train.d)
                  list(tree=err(predict(v.tree.d, v.test.d, type="c"),
                                v.test.d$Class),
                       nb=err(predict(v.nb.d, v.test.d), v.test.d$Class))
                })

  # misclassification error values for the Glass data
g.err <- lapply(g.disc,
                function(dm)
                {
                  g.train.d <- predict(dm, g.train)
                  g.test.d <- predict(dm, g.test)
                  g.tree.d <- rpart(Type~., g.train.d)
                  g.nb.d <- naiveBayes(Type~., g.train.d)
                  list(tree=err(predict(g.tree.d, g.test.d, type="c"),
                                g.test.d$Type),
                       nb=err(predict(g.nb.d, g.test.d), g.test.d$Type))
                })

  # error comparison
v.tree.err <- sapply(v.err, function(e) e$tree)
g.tree.err <- sapply(g.err, function(e) e$tree)
v.nb.err <- sapply(v.err, function(e) e$nb)
g.nb.err <- sapply(g.err, function(e) e$nb)

barplot(v.tree.err, main="Vehicle Silhouettes, rpart", ylab="Error", las=2)
lines(c(0, 17), rep(v.tree.err[1], 2), lty=2)

barplot(g.tree.err, main="Glass, rpart", ylab="Error", las=2)
lines(c(0, 17), rep(g.tree.err[1], 2), lty=2)

barplot(v.nb.err, main="Vehicle Silhouettes, naiveBayes", ylab="Error", las=2)
lines(c(0, 17), rep(v.nb.err[1], 2), lty=2)

barplot(g.nb.err, main="Glass, naiveBayes", ylab="Error", las=2)
lines(c(0, 17), rep(g.nb.err[1], 2), lty=2)
