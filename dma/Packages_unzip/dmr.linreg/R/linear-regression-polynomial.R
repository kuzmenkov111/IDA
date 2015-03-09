## polynomial representation enhancement
enhance.poly <- function(data, p=2)
{ do.call(cbind, lapply(1:p, function(j) data^j)) }

## polynomial regression representation function
repf.poly <- function(p=2)
{ repf.enh(function(data) enhance.poly(data, p), repf.linear) }

## polynomial regression representation function gradient
grad.poly <- function(p=2)
{ grad.enh(function(data) enhance.poly(data, p), grad.linear) }


if (FALSE)
{

  # gradient descent polynomial regression estimation for f3
gd3p <- gradient.descent(f3~a1+a2+a3+a4, lrdat.train, w=rep(0, 9),
                         repf=repf.poly(p=2), grad=grad.poly(p=2),
                         beta=0.001, eps=0.005)
  # test set error
mse(predict(gd3p$model, lrdat.test[,1:4]), lrdat.test$f3)

  # OLS polynomial regression estimation for f3
ols3p <- ols.enh(f3~a1+a2+a3+a4, lrdat.train, enhance.poly)
  # test set error
mse(predict(ols3p, lrdat.test[,1:4]), lrdat.test$f3)

}
