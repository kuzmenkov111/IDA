confmat <- function(pred.y, true.y)
{ table(pred.y, true.y, dnn=c("predicted", "true")) }


if (FALSE)
{

s.cm <- confmat(predict(s.tree, s.test, type="c"), s.test$Class)
  # error
(sum(s.cm)-sum(diag(s.cm)))/(sum(s.cm))
  # mean misclassification cost
sum(s.cm*s.r4test)/sum(s.cm)

}
