auc <- function(roc)
{ n <- nrow(roc); sum((roc$tpr[1:n-1]+roc$tpr[2:n])*diff(roc$fpr)/2) }

  # area under the ROC curve for the decision tree model
auc(s01.roc)
  # area under the ROC curve for a random model
auc(s01rand.roc)
