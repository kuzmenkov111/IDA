werr <- function(pred.y, true.y, w=rep(1, length(true.y)))
{ weighted.mean(pred.y!=true.y, w) }


if (FALSE)
{

  # double weight for the least frequent class
s.w2test <- ifelse(s.test$Class=="herbicide-injury", 2, 1)
werr(predict(s.tree, s.test, type="c"), s.test$Class, s.w2test)

  # random per-class weights 1..5
s.wctest <- round(runif(nlevels(s.test$Class), min=1, max=5))
s.w3test <- s.wctest[s.test$Class]
werr(predict(s.tree, s.test, type="c"), s.test$Class, s.w3test)

}
