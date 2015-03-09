set.seed(12)

  # generate artificial data
prdat <- data.frame(a1=floor(runif(400, min=1, max=5)),
                    a2=floor(runif(400, min=1, max=5)),
                    a3=floor(runif(400, min=1, max=5)),
                    a4=floor(runif(400, min=1, max=5)))
prdat$f <- 2*tanh(prdat$a1-2*prdat$a2+3*prdat$a3-prdat$a4+1)-
           3*tanh(-2*prdat$a1+3*prdat$a2-2*prdat$a3+prdat$a4-1)+2

  # training and test subsets
prdat.train <- prdat[1:200,]
prdat.test <- prdat[201:400,]
