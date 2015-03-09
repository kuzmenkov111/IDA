## random naive Bayes ensemble modeling using m base models
## each with ns randomly selected attributes
## (if unspecified, it defaults to the square root of the number of attributes)
randnaiveBayes <- function(formula, data, m, ns=0)
{
  attributes <- x.vars(formula, data)
  target <- y.var(formula)
  ns <- ifelse(ns==0, round(sqrt(length(attributes))),
                      clip.val(ns, 1, length(attributes)))

  `class<-`(lapply(1:m, function(i)
                        {
                          bag <- sample(nrow(data), size=nrow(data), replace=TRUE)
                          sa <- sample(length(attributes), ns)
                          naiveBayes(make.formula(target, attributes[sa]),
                                     data[bag,])
                        }), "randnaiveBayes")
}

## random naive Bayes prediction
predict.randnaiveBayes <- function(rnb, data, prob=FALSE)
{
  predict.ensemble.prob(rnb, data, predf=function(...) predict(..., type="r"),
                        prob=prob, labels=rnb[[1]]$levels)
}

  # random naive Bayes for the HouseVotes84 data
hv.rnb <- randnaiveBayes(Class~., hv.train, 500)
hv.pred.rnb <- predict(hv.rnb, hv.test)
  # random naive Bayes test set error for the HouseVotes84 data
hv.err.rnb <- list(nb = err(hv.pred.rnb, hv.test$Class))
