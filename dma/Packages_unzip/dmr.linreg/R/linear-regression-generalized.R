## generalized representation function
repf.gen <- function(link.inv, repf=repf.linear)
{ function(data, w) { link.inv(repf(data, w)) } }

## generalized representation function gradient
grad.gen <- function(link.inv.deriv, repf=repf.linear, grad=grad.linear)
{ function(data, w) { rmm(grad(data, w), link.inv.deriv(repf(data, w))) } }


if (FALSE)
{

  # perfect inverse link function for f2
link2.inv <- function(v) { tanh(v/10) }
  # and its derivative
link2.inv.deriv <- function(v) { (1-tanh(v/10)^2)/10 }

  # perfect generalized linear representation function for f2
repf.gen2 <- repf.gen(link2.inv)
  # and its gradient
grad.gen2 <- grad.gen(link2.inv.deriv)

  # gradient descent estimation of generalized linear model parameters for f2
gd2g <- gradient.descent(f2~a1+a2+a3+a4, lrdat.train, w=rep(0, 5),
                         repf=repf.gen2, grad=grad.gen2,
                         beta=0.5, eps=0.0001)
  # test set error
mse(predict(gd2g$model, lrdat.test[,1:4]), lrdat.test$f2)

}
