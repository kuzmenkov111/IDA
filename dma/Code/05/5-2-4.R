## threshold representation function
repf.threshold <- function(repf) { function(data, w) ustep(repf(data, w)) }

  # "perfect" threshold model
perf.threshold <- `class<-`(list(repf=repf.threshold(repf.linear), w=w.perf), "par")
  # test set error
err(predict(perf.threshold, lcdat.test[,1:4]), lcdat.test$c)
