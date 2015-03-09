mae <- function(pred.y, true.y) { mean(abs(true.y-pred.y)) }


if (FALSE)
{

mae(predict(bh.tree, bh.test), bh.test$medv)

}
