  # example target function
rtf <- function(a1, a2) { sin(a1+a2)/(a1+a2) }

  # artificial dataset
rtdat <- data.frame(a1=floor(runif(300, min=1, max=6)),
                    a2=floor(runif(300, min=1, max=6)))
rtdat$f <- rtf(rtdat$a1, rtdat$a2)

  # regression tree
rtf.rp <- rpart(f~., rtdat)
  # target function predictions
rtf.p <- function(a1, a2) { predict(rtf.rp, data.frame(a1, a2)) }

  # 3D plots
par(mfrow=1:2, mar=rep(0.1, 4))
a1 <- a2 <- seq(1, 5, 0.1)
  # true f
persp(a1, a2, outer(a1, a2, rtf), zlab="true f", theta=30, phi=30, col="grey")
  # predicted f
persp(a1, a2, outer(a1, a2, rtf.p), zlab="predicted f", theta=30, phi=30, col="grey")
