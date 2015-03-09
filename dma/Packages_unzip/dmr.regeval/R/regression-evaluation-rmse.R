rmse <- function(pred.y, true.y) { sqrt(mse(pred.y, true.y)) }


if (FALSE)
{

rmse(predict(bh.tree, bh.test), bh.test$medv)

}
