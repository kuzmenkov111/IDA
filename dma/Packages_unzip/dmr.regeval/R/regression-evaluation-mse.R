mse <- function(pred.y, true.y) { mean((true.y-pred.y)^2) }


if (FALSE)
{

mse(predict(bh.tree, bh.test), bh.test$medv)

}
