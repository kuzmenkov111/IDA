  # the commented line runs a 200-repetition experiment, which takes a long time
#bh.ebv <- eval.bias.var(rpart, medv~., BostonHousing, perf=mse, wperf=wmse, n=200)
  # this can be used for a quick illustration
bh.ebv <- eval.bias.var(rpart, medv~., BostonHousing, perf=mse, wperf=wmse, n=10)

boxplot(bh.ebv$performance[,-1], main="Error", las=2)
lines(c(0, 13), rep(mean(bh.ebv$performance[,1]), 2), lty=2)
barplot(bh.ebv$bias, main="Bias", las=2)
barplot(bh.ebv$variance, main="Variance", las=2)
