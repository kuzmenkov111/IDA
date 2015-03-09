## assign class labels according to the given cutoff value
cutclass <- function(s, cutoff, labels) { factor(ustep(s, cutoff), labels=labels) }

## identify the best cutoff value
## satisfying the minimum tpr or maximum fpr constraint
roc.shift <- function(r, min.tpr=NULL, max.fpr=NULL)
{
  if (!is.null(min.tpr))
    max(r$cutoff[r$tpr>=min.tpr])
  else if (!is.null(max.fpr))
    min(r$cutoff[r$fpr<=max.fpr])
  else
    0.5
}


if (FALSE)
{

  # shift to achieve tpr>0.85 at minimum fpr
s01.t085 <- roc.shift(s01.roc, min.tpr=0.85)
s01.cm.t085 <- confmat(cutclass(predict(s01.tree, s01.test)[,2],
                                s01.t085, s01.labels),
                       s01.test$Class)
  # shift to achieve maximum tpr at fpr<0.5
s01.f05 <- roc.shift(s01.roc, max.fpr=0.5)
s01.cm.f05 <- confmat(cutclass(predict(s01.tree, s01.test)[,2], s01.f05, s01.labels),
                      s01.test$Class)
  # the ROC curve
plot(s01.roc$fpr, s01.roc$tpr, type="l", col="blue", xlab="FP rate", ylab="TP rate")
  # the default operating point
points(fpr(s01.cm), tpr(s01.cm), col="red")
  # the shifted operating points
points(fpr(s01.cm.t085), tpr(s01.cm.t085), col="green")
points(fpr(s01.cm.f05), tpr(s01.cm.f05), col="green")

}
