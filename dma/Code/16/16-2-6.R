  # soft-margin SVM
svm.ms.1 <- svm.linear(c~., kmdat.m, solver="ipop", cost=1)
w.ms.1 <- svm.ms.1$model$w

svm.ms.01 <- svm.linear(c~., kmdat.m, solver="ipop", cost=0.1)
w.ms.01 <- svm.ms.01$model$w

  # soft margin: geometric margin corresponding to functional margin of 1
1/l2norm(w.ms.1[-length(w.ms.1)])
1/l2norm(w.ms.01[-length(w.ms.01)])

  # separating and margin lines for cost=1
plot.margin(w.ms.1, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1, main="Linearly separable")
  # separating and margin lines for cost=0.1
plot.margin(w.ms.01, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1, add=TRUE, lty=3)

  # the same for linearly inseparable data

kmdat.m.nls <- kmdat.m
kmdat.m.nls$c <- as.factor(ifelse(runif(nrow(kmdat.m))<0.1,
                                  1-as.numchar(kmdat.m$c), as.numchar(kmdat.m$c)))

svm.ms.nls.1 <- svm.linear(c~., kmdat.m.nls, solver="ipop", cost=1)
w.ms.nls.1 <- svm.ms.nls.1$model$w

svm.ms.nls.01 <- svm.linear(c~., kmdat.m.nls, solver="ipop", cost=0.1)
w.ms.nls.01 <- svm.ms.nls.01$model$w

  # soft margin: geometric margin corresponding to functional margin of 1
1/l2norm(w.ms.nls.1[-length(w.ms.nls.1)])
1/l2norm(w.ms.nls.01[-length(w.ms.nls.01)])

  # separating and margin lines for cost=1
plot.margin(w.ms.nls.1, kmdat.m.nls[,1:2], 2*as.num0(kmdat.m.nls$c)-1,
            main="Linearly inseparable")
  # separating and margin lines for cost=0.1
plot.margin(w.ms.nls.01, kmdat.m.nls[,1:2], 2*as.num0(kmdat.m.nls$c)-1,
            add=TRUE, lty=3)
