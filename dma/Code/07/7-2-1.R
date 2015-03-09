err <- function(pred.y, true.y) { mean(pred.y!=true.y) }

err(predict(s.tree, s.test, type="c"), s.test$Class)
