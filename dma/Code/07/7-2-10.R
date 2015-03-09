  # predicted scores and true class labels
sctab <- data.frame(score=c(1, 1, 3, 4, 5, 5, 6, 7, 9, 9),
                    class=factor(c(0, 0, 1, 0, 0, 1, 1 ,0, 1, 1)))

  # operating point identification
scroc <- t(sapply(c(sort(unique(sctab$score)), Inf),
                  function(sc)
                  {
                    pred <- factor(as.numeric(sctab$score<sc),
                                   levels=levels(sctab$class))
                    list(tpr=tpr(cm <- confmat(pred, sctab$class)),
                         fpr=fpr(cm))
                  }))

  # ROC curve
plot(scroc, type="l", xlab="FP rate", ylab="TP rate")
