## linear representation function
repf.linear <- function(data, w)
{ rowSums(cmm(data, w[1:(n <- ncol(data))])) + w[n+1] }

  # perfect parameter vector for f1
w.perf1 <- c(3, 4, -2, 2, -3)
  # perfect model for f1
mod.perf1 <- `class<-`(list(w=w.perf1, repf=repf.linear), "par")
  # test set error
mse(predict(mod.perf1, lrdat.test[,1:4]), lrdat.test$f1)
