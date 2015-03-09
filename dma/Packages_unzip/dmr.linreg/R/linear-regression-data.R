set.seed(12)

  # generate artificial data
lrdat <- data.frame(a1=floor(runif(400, min=1, max=5)),
                    a2=floor(runif(400, min=1, max=5)),
                    a3=floor(runif(400, min=1, max=5)),
                    a4=floor(runif(400, min=1, max=5)))
lrdat$f1 <- 3*lrdat$a1+4*lrdat$a2-2*lrdat$a3+2*lrdat$a4-3
lrdat$f2 <- tanh(lrdat$f1/10)
lrdat$f3 <- lrdat$a1^2+2*lrdat$a2^2-lrdat$a3^2-2*lrdat$a4^2+
            2*lrdat$a1-3*lrdat$a2+2*lrdat$a3-3*lrdat$a4+1
lrdat$f4 <- 2*tanh(lrdat$a1-2*lrdat$a2+3*lrdat$a3-lrdat$a4+1)-
            3*tanh(-2*lrdat$a1+3*lrdat$a2-2*lrdat$a3+lrdat$a4-1)+2

  # training and test subsets
lrdat.train <- lrdat[1:200,]
lrdat.test <- lrdat[201:400,]
