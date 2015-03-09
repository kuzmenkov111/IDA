wroc <- function(pred.s, true.y, w=rep(1, length(true.y)))
{
  cutoff <- Inf  # start with all instances classified as negative
  tp <- fp <- 0
  tn <- sum((2-as.integer(true.y))*w)  # all negative instances
  fn <- sum((as.integer(true.y)-1)*w)  # all positive instances
  rt <- data.frame()

  sord <- order(pred.s, decreasing=TRUE)  # score ordering
  for (i in 1:length(sord))
  {
    if (pred.s[sord[i]] < cutoff)
    {
      rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn), cutoff=cutoff))
      cutoff <- pred.s[sord[i]]
    }

    p <- (as.integer(true.y[sord[i]])-1)*w[sord[i]]  # next positive
    n <- (2-as.integer(true.y[sord[i]]))*w[sord[i]]  # next negative
    tp <- tp+p
    fp <- fp+n
    tn <- tn-n
    fn <- fn-p
  }
  rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn), cutoff=cutoff))
}


if (FALSE)
{

  # ROC curve with double weight for the brown-spot class
s01.w1roc <- wroc(predict(s01.tree, s01.test)[,2], s01.test$Class, s01.w1test)
plot(s01.w1roc$fpr, s01.w1roc$tpr, type="l", col="blue", xlab="FP rate", ylab="TP rate")
auc(s01.w1roc)

  # ROC curve with 10 times less weight for instances with plant.stand=1
s01.w2roc <- wroc(predict(s01.tree, s01.test)[,2], s01.test$Class, s01.w2test)
lines(s01.w2roc$fpr, s01.w2roc$tpr, col="red")
legend("bottomright", c("brown-spot x2", "plant.stand=1 x10"),
       col=c("blue", "red"), lty=1)
auc(s01.w2roc)

}
