r2 <- function(pred.y, true.y)
{ 1 - length(true.y)*mse(pred.y, true.y)/((length(true.y)-1)*var(true.y)) }


if (FALSE)
{

r2(predict(bh.tree, bh.test), bh.test$medv)

}
