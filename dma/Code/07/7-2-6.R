tpr <- function(cm) { if (is.nan(p <- cm[2,2]/(cm[2,2]+cm[1,2]))) 1 else p }
fpr <- function(cm) { if (is.nan(p <- cm[2,1]/(cm[2,1]+cm[1,1]))) 1 else p }
precision <- function(cm) { if (is.nan(p <- cm[2,2]/(cm[2,2]+cm[2,1]))) 1 else p }
recall <- tpr
sensitivity <- tpr
specificity <- function(cm) { if (is.nan(p <- cm[1,1]/(cm[2,1]+cm[1,1]))) 1 else p }

s01.cm <- confmat(predict(s01.tree, s01.test, type="c"), s01.test$Class)

list(tpr=tpr(s01.cm),
     fpr=fpr(s01.cm),
     precision=precision(s01.cm),
     recall=recall(s01.cm),
     sensitivity=sensitivity(s01.cm),
     specificity=specificity(s01.cm))
