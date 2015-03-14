### foreach
library(parallel)
library(foreach)
x = foreach(i=1:3) %do% sqrt(i)
x

x = foreach(a=1:3, b=rep(10,3)) %do% (a + b)
x = foreach(a=1:3, b=rep(10,3)) %do% {
  a + b
}
# iteration to min(length(a), length(b), ...)
x = foreach(a=1:100, b=rep(10,3)) %do% (a + b)

# .combine option
x = foreach(a=1:3, .combine="c") %do% exp(a)
x = foreach(a=1:3, .combine="rbind") %do% exp(a)
x = foreach(a=1:3, .combine="+") %do% exp(a)
x = foreach(a=1:3, .combine="min") %do% exp(a)
comMin = function(a, b) if(a < b) a else b
x = foreach(a=1:3, .combine="comMin") %do% exp(a)
comMean = function(a, b) {
  sum(a, b) / length(cbind(a, b))
}
x = foreach(a=1:3, .combine="comMean") %do% exp(a)







# http://adv-r.had.co.nz/Functionals.html
# check map, reduce, filter