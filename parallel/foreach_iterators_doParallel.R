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
<<<<<<< HEAD
comMin = function(a, b) if(a < b) a else b
x = foreach(a=1:3, .combine="comMin") %do% exp(a)
comMean = function(a, b) {
  sum(a, b) / length(cbind(a, b))
}
x = foreach(a=1:3, .combine="comMean") %do% exp(a)



=======
x = foreach(a=1:4, .combine="cbind") %do% mean(rnorm(4))

cfun = function(a, b) NULL
x = foreach(a=1:4, .combine="cfun") %do% rnorm(4)

cfun = function(...) NULL
x = foreach(a=1:4, .combine="cfun", .multicombine=TRUE) %do% rnorm(4)
x = foreach(a=1:4, .combine="cfun", .multicombine=TRUE, .maxcombine=10) %do% rnorm(4)
>>>>>>> 56eeddba21f06b242816e6a69a8f574c2a4a24c9




# http://adv-r.had.co.nz/Functionals.html
# check map, reduce, filter