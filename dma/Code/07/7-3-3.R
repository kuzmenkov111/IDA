leave1out <- function(alg, formula, data, args=NULL, predf=predict, prob=FALSE)
{
  yn <- as.character(formula)[2]  # class column name
  ylabs <- levels(data[[yn]])     # class labels
  pred.y <- NULL  # predictions
  true.y <- NULL  # true class labels

  for (i in 1:nrow(data))
  {
    train <- data[-i,]
    test <- data[i,]
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

  # leave-one-out for discrete predictions
s01l1o <- leave1out(rpart, Class~., Soybean01,
                    predf=function(...) predict(..., type="c"))
err(s01l1o$pred, s01l1o$true)
confmat(s01l1o$pred, s01l1o$true)

  # leave-one-out for probabilistic predictions
s01l1op <- leave1out(rpart, Class~., Soybean01,
                     predf=function(...) predict(..., type="p")[,2], prob=TRUE)
s01l1op.roc <- roc(s01l1op$pred, s01l1op$true)
plot(s01l1op.roc$fpr, s01l1op.roc$tpr, type="l", xlab="FP rate", ylab="TP rate")
auc(s01l1op.roc)
