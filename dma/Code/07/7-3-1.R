holdout <- function(alg, formula, data, args=NULL, predf=predict, prob=FALSE,
                    p=0.33, n=1)
{
  yn <- as.character(formula)[2]  # class column name
  ylabs <- levels(data[[yn]])     # class labels
  pred.y <- NULL  # predictions
  true.y <- NULL  # true class labels

  for (t in 1:n)
  {
    r <- runif(nrow(data))
    train <- data[r>=p,]
    test <- data[r<p,]
    model <- do.call(alg, c(list(formula, train), args))
    pred.y <- c(pred.y, predf(model, test))
    true.y <- c(true.y, test[[yn]])
  }

  if (!is.null(ylabs))
  {
    if (!prob)
      pred.y <- factor(pred.y, levels=1:length(ylabs), labels=ylabs)
    true.y <- factor(true.y, levels=1:length(ylabs), labels=ylabs)
  }

  return(data.frame(pred=pred.y, true=true.y))
}

  # hold-out evaluation of discrete predictions
s01ho <- holdout(rpart, Class~., Soybean01,
                 predf=function(...) predict(..., type="c"), n=10)
err(s01ho$pred, s01ho$true)
confmat(s01ho$pred, s01ho$true)

  # hold-out evaluation of probabilistic predictions
s01hop <- holdout(rpart, Class~., Soybean01,
                  predf=function(...) predict(..., type="p")[,2], prob=TRUE, n=10)
s01hop.roc <- roc(s01hop$pred, s01hop$true)
plot(s01hop.roc$fpr, s01hop.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(s01hop.roc)
