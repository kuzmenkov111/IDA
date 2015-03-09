## naive Bayes prediction for a single instance
predict1.nbc <- function(model, x)
{
  aind <- names(x) %in% names(model$cond)
  bnum <- model$prior*apply(mapply(function(a, v)
                                   model$cond[[a]][v,], names(model$cond), x[aind]),
                            1, prod)
  bnum/sum(bnum)
}


## naive Bayes prediction for a dataset
predict.nbc <- function(model, data)
{
  t(sapply(1:nrow(data), function(i) predict1.nbc(model, data[i,])))
}


if (FALSE)
{

  # naive Bayes predictions for the weather data
predict(nbw, weather)

}
