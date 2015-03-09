  # selected attribute subsets
v.self <- list(nosel=setdiff(names(v.train), "Class"),
               simple=v.sel.simple, cfs=v.sel.cfs, cons=v.sel.cons,
               rel=v.sel.rel, rf=v.sel.rf)
s.self <- list(nosel=setdiff(names(s.train), "Class"),
               simple=s.sel.simple, cfs=s.sel.cfs, cons=s.sel.cons,
               rel=s.sel.rel, rf=s.sel.rf)
bh.self <- list(nosel=setdiff(names(bh.train), "medv"),
                simple=bh.sel.simple, cfs=bh.sel.cfs, cons=bh.sel.cons,
                rel=bh.sel.rel, rf=bh.sel.rf)

  # misclassification error
v.errf <- sapply(v.self, function(sel)
                         {
                           tree <- rpart(make.formula("Class", sel), v.train)
                           err(predict(tree, v.test, type="c"), v.test$Class)
                         })
s.errf <- sapply(s.self, function(sel)
                         {
                           tree <- rpart(make.formula("Class", sel), s.train)
                           err(predict(tree, s.test, type="c"), s.test$Class)
                         })
bh.errf <- sapply(bh.self, function(sel)
                           {
                             tree <- rpart(make.formula("medv", sel), bh.train)
                             mse(predict(tree, bh.test), bh.test$medv)
                           })

  # attribute subset size
v.sizef <- sapply(v.self, length)
s.sizef <- sapply(s.self, length)
bh.sizef <- sapply(bh.self, length)

barplot(v.errf, main="Vehicle Silhouettes", ylab="Error", las=2)
lines(c(0, 8), rep(v.errf[1], 2), lty=2)
barplot(v.sizef, main="Vehicle Silhouettes", ylab="Size", las=2)

barplot(s.errf, main="Soybean", ylab="Error", las=2)
lines(c(0, 8), rep(s.errf[1], 2), lty=2)
barplot(s.sizef, main="Soybean", ylab="Size", las=2)

barplot(bh.errf, main="Boston Housing", ylab="Error", las=2)
lines(c(0, 8), rep(bh.errf[1], 2), lty=2)
barplot(bh.sizef, main="Boston Housing", ylab="Size", las=2)
