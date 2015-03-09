  # selected attribute subsets
v.selw <- list(nosel=setdiff(names(v.train), "Class"),
               fwd=v.sel.fwd$subset, bwd=v.sel.bwd$subset, flt=v.sel.flt$subset)
s.selw <- list(nosel=setdiff(names(s.train), "Class"),
               fwd=s.sel.fwd$subset, bwd=s.sel.bwd$subset, flt=s.sel.flt$subset)
bh.selw <- list(nosel=setdiff(names(bh.train), "medv"),
                fwd=bh.sel.fwd$subset, bwd=bh.sel.bwd$subset, flt=bh.sel.flt$subset)

  # misclassification error
v.errw <- sapply(v.selw, function(sel)
                         {
                           tree <- rpart(make.formula("Class", sel), v.train)
                           err(predict(tree, v.test, type="c"), v.test$Class)
                         })
s.errw <- sapply(s.selw, function(sel)
                         {
                           tree <- rpart(make.formula("Class", sel), s.train)
                           err(predict(tree, s.test, type="c"), s.test$Class)
                         })
bh.errw <- sapply(bh.selw, function(sel)
                           {
                             tree <- rpart(make.formula("medv", sel), bh.train)
                             mse(predict(tree, bh.test), bh.test$medv)
                           })

  # attribute subset size
v.sizew <- sapply(v.selw, length)
s.sizew <- sapply(s.selw, length)
bh.sizew <- sapply(bh.selw, length)

barplot(v.errw, main="Vehicle Silhouettes", ylab="Error", las=2)
lines(c(0, 5), rep(v.errw[1], 2), lty=2)
barplot(v.sizew, main="Vehicle Silhouettes", ylab="Size", las=2)

barplot(s.errw, main="Soybean", ylab="Error", las=2)
lines(c(0, 5), rep(s.errw[1], 2), lty=2)
barplot(s.sizew, main="Soybean", ylab="Size", las=2)

barplot(bh.errw, main="Boston Housing", ylab="Error", las=2)
lines(c(0, 5), rep(bh.errw[1], 2), lty=2)
barplot(bh.sizew, main="Boston Housing", ylab="Size", las=2)
