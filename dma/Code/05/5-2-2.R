lcdat.train.lr <- lcdat.train[,1:4]
lcdat.train.lr$g <- lcg(lcdat.train$a1, lcdat.train$a2,
                        lcdat.train$a3, lcdat.train$a4)

  # "perfect" parameter vector
w.perf <- lm(g~., lcdat.train.lr)$coef[c(2:5, 1)]

  # "perfect" predictions
mse(predict.par(list(repf=repf.linear, w=w.perf), lcdat[,1:4]),
    lcg(lcdat$a1, lcdat$a2, lcdat$a3, lcdat$a4))
