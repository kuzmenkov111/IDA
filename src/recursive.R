power <- function(x, n) {
  if(n >= 1) x * power(x, n-1)
  else 1
}

## error to run power 10000 times with defalut option
options()$expressions
#[1] 5000

power(2, 1e3)
#[1] 1.071509e+301

power(2, 1e4)
#Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
#Error during wrapup: evaluation nested too deeply: infinite recursion / options(expressions=)?

## expressions parameter updated and results in stack overflow
options(expressions = 100000)
power(2, 1e4)
#Error: protect(): protection stack overflow

## tail recursion supported in Scala
power.rec <- function(x, n, t = 1) {
  if(n < 1) t
  else power.rec(x, n-1, x*t)
}

# turn to default and even worse
options(expressions = 5000)
power.rec(2, 1e3)
#Error: evaluation nested too deeply: infinite recursion / options(expressions=)?
#Error during wrapup: evaluation nested too deeply: infinite recursion / options(expressions=)?

# works in updated parameter
options(expressions = 100000)
power.rec(2, 1e3)
#[1] 1.071509e+301

# stack overflow error again
power.rec(2, 1e4)
#Error: protect(): protection stack overflow

