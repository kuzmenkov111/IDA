  # hard-margin SVM
svm.mh <- svm.linear.prim(c~., kmdat.m, solver="ipop")

  # optimal separating and margin lines
plot.margin(svm.mh$model$w, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1)

  # suboptimal separating and margin lines for comparison
plot.margin(w.m, kmdat.m[,1:2], 2*as.num0(kmdat.m$c)-1, add=TRUE, lty=3)
