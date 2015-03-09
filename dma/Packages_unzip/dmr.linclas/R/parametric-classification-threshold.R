## threshold representation function
repf.threshold <- function(repf) { function(data, w) ustep(repf(data, w)) }

## threshold representation gradient (assuming 1)
grad.threshold <- function(grad) { grad }


if (FALSE)
{

  # perfect threshold model
perf.threshold <- `class<-`(list(repf=repf.threshold(repf.perf), w=w.perf), "par")
  # test set error
err(predict(perf.threshold, pcdat.test[,1:4]), pcdat.test$c)

}
