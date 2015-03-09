rae <- function(pred.y, true.y)
{ mae(pred.y, true.y)/mean(abs(true.y-mean(true.y))) }


if (FALSE)
{

rae(predict(bh.tree, bh.test), bh.test$medv)

}
