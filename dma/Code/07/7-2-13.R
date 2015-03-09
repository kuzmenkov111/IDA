mixclass <- function(c1, c2, p)
{ factor(ifelse(p<runif(length(c1)), c1, c2), labels=levels(c1)) }

  # interpolate between the two shifted operating points
s01.mix <- mixclass(cutclass(predict(s01.tree, s01.test)[,2], s01.t085, s01.labels),
                    cutclass(predict(s01.tree, s01.test)[,2], s01.f05, s01.labels),
                    0.75)
s01.cmi <- confmat(s01.mix, s01.test$Class)

  # the ROC curve
plot(s01.roc$fpr, s01.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
  # the default operating point
points(fpr(s01.cm), tpr(s01.cm), pch=8)
  # the 1st shifted operating point
points(fpr(s01.cm.t085), tpr(s01.cm.t085), pch=1)
  # the 2nd shifted operating point
points(fpr(s01.cm.f05), tpr(s01.cm.f05), pch=2)
  # the interpolated operating point
points(fpr(s01.cmi), tpr(s01.cmi), pch=5)
