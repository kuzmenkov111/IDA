crossval <- function(alg, formula, data, args=NULL, predf=predict, prob=FALSE,
                     k=10, n=1)
{
  yn <- as.character(formula)[2]  # class column name
  ylabs <- levels(data[[yn]])     # class labels
  pred.y <- NULL  # predictions
  true.y <- NULL  # true class labels

  for (t in 1:n)
  {
    ind <- sample(k, size=nrow(data), replace=TRUE)  # index of k random subsets
    for (i in 1:k)
    {
      train <- data[ind!=i,]
      test <- data[ind==i,]
      model <- do.call(alg, c(list(formula, train), args))
      pred.y <- c(pred.y, predf(model, test))
      true.y <- c(true.y, test[[yn]])
    }
  }

  if (!is.null(ylabs))
  {
    if (!prob)
      pred.y <- factor(pred.y, levels=1:length(ylabs), labels=ylabs)
    true.y <- factor(true.y, levels=1:length(ylabs), labels=ylabs)
  }

  return(data.frame(pred=pred.y, true=true.y))
}


if (FALSE)
{

  # 3-fold cross-validation for discrete predictions
s01cv3 <- crossval(rpart, Class~., Soybean01,
                   predf=function(...) predict(..., type="c"), k=3)
err(s01cv3$pred, s01cv3$true)
confmat(s01cv3$pred, s01cv3$true)

  # 10-fold cross-validation for discrete predictions
s01cv10 <- crossval(rpart, Class~., Soybean01,
                    predf=function(...) predict(..., type="c"), k=10)
err(s01cv10$pred, s01cv10$true)
confmat(s01cv10$pred, s01cv10$true)

  # 20-fold cross-validation for discrete predictions
s01cv20 <- crossval(rpart, Class~., Soybean01,
                    predf=function(...) predict(..., type="c"), k=20)
err(s01cv20$pred, s01cv20$true)
confmat(s01cv20$pred, s01cv20$true)

  # 4x5-fold cross-validation for discrete predictions
s01cv4x5 <- crossval(rpart, Class~., Soybean01,
                     predf=function(...) predict(..., type="c"), k=5, n=4)
err(s01cv20$pred, s01cv4x5$true)
confmat(s01cv4x5$pred, s01cv4x5$true)

  # 10-fold cross-validation for probabilistic predictions
s01cv10p <- crossval(rpart, Class~., Soybean01,
                     predf=function(...) predict(..., type="p")[,2], prob=TRUE, k=10)
s01cv10p.roc <- roc(s01cv10p$pred, s01cv10p$true)
plot(s01cv10p.roc$fpr, s01cv10p.roc$tpr, type="l", col="blue",
     xlab="FP rate", ylab="TP rate")
auc(s01cv10p.roc)

}
