roc <- function(pred.s, true.y)
{
  cutoff <- Inf  # start with all instances classified as negative
  tp <- fp <- 0
  tn <- sum(2-as.integer(true.y))  # all negative instances
  fn <- sum(as.integer(true.y)-1)  # all positive instances
  rt <- data.frame()

  sord <- order(pred.s, decreasing=TRUE)  # score ordering
  for (i in 1:length(sord))
  {
    if (pred.s[sord[i]] < cutoff)
    {
      rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn), cutoff=cutoff))
      cutoff <- pred.s[sord[i]]
    }

    p <- as.integer(true.y[sord[i]])-1  # next positive classified as positive
    n <- 2-as.integer(true.y[sord[i]])  # next negative classified as positive
    tp <- tp+p
    fp <- fp+n
    tn <- tn-n
    fn <- fn-p
  }
  rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn), cutoff=cutoff))
}

if (FALSE)
{

  # ROC curve for the decision tree model
s01.roc <- roc(predict(s01.tree, s01.test)[,2], s01.test$Class)
plot(s01.roc$fpr, s01.roc$tpr, type="l", col="blue", xlab="FP rate", ylab="TP rate")

  # ROC curve for a random model
s01rand <- runif(nrow(s01.test))
s01rand.roc <- roc(s01rand, s01.test$Class)
lines(s01rand.roc$fpr, s01rand.roc$tpr, col="black")

}
