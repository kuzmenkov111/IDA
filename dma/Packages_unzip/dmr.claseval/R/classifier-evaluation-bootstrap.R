bootstrap <- function(alg, formula, data, args=NULL, predf=predict, prob=FALSE,
                      w=0.632, m=100)
{
  yn <- as.character(formula)[2]  # class column name
  ylabs <- levels(data[[yn]])     # class labels
  pred.y.w <- NULL  # predictions
  true.y.w <- NULL  # true class labels

  for (t in 1:m)
  {
    bag <- sample(nrow(data), size=nrow(data), replace=TRUE)
    train <- data[bag,]
    test <- data[-bag,]
    model <- do.call(alg, c(list(formula, train), args))
    pred.y.w <- c(pred.y.w, predf(model, test))
    true.y.w <- c(true.y.w, test[[yn]])
  }

  if (w<1)
  {
    model <- do.call(alg, c(list(formula, data), args))
    pred.y.1w <- predf(model, data)
    true.y.1w <- data[[yn]]
    w <- c(rep(w/m, length(pred.y.w)), rep(1-w, nrow(data)))
  }
  else
  {
    pred.y.1w <- true.y.1w <- NULL
    w <- rep(w/m, length(pred.y.w))
  }

  pred.y <- c(pred.y.w, pred.y.1w)
  true.y <- c(true.y.w, true.y.1w)

  if (!is.null(ylabs))
  {
    if (!prob)
      pred.y <- factor(pred.y, levels=1:length(ylabs), labels=ylabs)
    true.y <- factor(true.y, levels=1:length(ylabs), labels=ylabs)
  }

  return(data.frame(pred=pred.y, true=true.y, w=w))
}


if (FALSE)
{

  # 20x bootstrap for discrete predictions
s01bs20 <- bootstrap(rpart, Class~., Soybean01,
                     predf=function(...) predict(..., type="c"), w=1, m=20)
err(s01bs20$pred, s01bs20$true)
confmat(s01bs20$pred, s01bs20$true)

  # 20x .632 bootstrap for discrete predictions
s01.632bs20 <- bootstrap(rpart, Class~., Soybean01,
                         predf=function(...) predict(..., type="c"), m=20)
  # weighted error
werr(s01.632bs20$pred, s01.632bs20$true, s01.632bs20$w)
  # weighted confusion matrix
wconfmat(s01.632bs20$pred, s01.632bs20$true, s01.632bs20$w)

  # 20x bootstrap for probabilistic predictions
s01bs20p <- bootstrap(rpart, Class~., Soybean01,
                      predf=function(...) predict(..., type="p")[,2], prob=TRUE,
                      w=1, m=20)
s01bs20p.roc <- roc(s01bs20p$pred, s01bs20p$true)
plot(s01bs20p.roc$fpr, s01bs20p.roc$tpr, type="l", col="blue",
     xlab="FP rate", ylab="TP rate")
auc(s01bs20p.roc)

  # 20x .632 bootstrap for probabilistic predictions
s01.632bs20p <- bootstrap(rpart, Class~., Soybean01,
                          predf=function(...) predict(..., type="p")[,2], prob=TRUE,
                          m=20)
  # weighted ROC
s01.632bs20p.roc <- wroc(s01.632bs20p$pred, s01.632bs20p$true, s01.632bs20p$w)
lines(s01.632bs20p.roc$fpr, s01.632bs20p.roc$tpr, col="red")
legend("bottomright", c("plain", ".632"), col=c("blue", "red"), lty=1)
auc(s01.632bs20p.roc)

}
