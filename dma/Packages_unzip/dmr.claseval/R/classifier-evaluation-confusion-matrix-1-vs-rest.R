## per-class 1 vs. rest confusion matrices
confmat01 <- function(pred.y, true.y)
{
  `names<-`(lapply(levels(true.y),
                   function(d)
                   {
                     cm <- confmat(factor(as.integer(pred.y==d), levels=0:1),
                                   factor(as.integer(true.y==d), levels=0:1))
                   }), levels(true.y))
}


if (FALSE)
{

s.cm01 <- confmat01(predict(s.tree, s.test, type="c"), s.test$Class)
  # average TP rate, FP rate, and f-measure
rowMeans(sapply(s.cm01, function(cm) c(tpr=tpr(cm), fpr=fpr(cm), fm=f.measure(cm))))

}
