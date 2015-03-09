eval.bias.var <- function(alg, formula, data, args=NULL, predf=predict,
                          perf=err, wperf=werr, p=0.66, n=100)
{
  yn <- as.character(formula)[2]  # class column name
  performance <- data.frame()
  for (i in 1:n)
  {
    r <- runif(nrow(data))
    data.avail <- data[r<p,]   # pretend this is the available dataset
    data.new <- data[r>=0.7,]  # pretend this a new dataset
    model <- do.call(alg, c(list(formula, data.avail), args))

    cv3 <- crossval(alg, formula, data.avail, args=args, predf=predf, k=3)
    cv5 <- crossval(alg, formula, data.avail, args=args, predf=predf, k=5)
    cv10 <- crossval(alg, formula, data.avail, args=args, predf=predf, k=10)
    cv20 <- crossval(alg, formula, data.avail, args=args, predf=predf, k=20)
    cv5x4 <- crossval(alg, formula, data.avail, args=args, predf=predf, k=5, n=4)
    ho <- holdout(alg, formula, data.avail, args=args, predf=predf)
    hox10 <- holdout(alg, formula, data.avail, args=args, predf=predf, n=10)
    l1o <- leave1out(alg, formula, data.avail, args=args, predf=predf)
    bs10 <- bootstrap(alg, formula, data.avail, args=args, predf=predf, w=1, m=10)
    bs50 <- bootstrap(alg, formula, data.avail, args=args, predf=predf, w=1, m=50)
    bs10.632 <- bootstrap(alg, formula, data.avail, args=args, predf=predf, m=10)
    bs50.632 <- bootstrap(alg, formula, data.avail, args=args, predf=predf, m=50)

    performance <- rbind(performance,
                         data.frame(perf(predf(model, data.new), data.new[[yn]]),
                                    perf(cv3$pred, cv3$true),
                                    perf(cv5$pred, cv5$true),
                                    perf(cv10$pred, cv10$true),
                                    perf(cv20$pred, cv20$true),
                                    perf(cv5x4$pred, cv5x4$true),
                                    perf(ho$pred, ho$true),
                                    perf(hox10$pred, hox10$true),
                                    perf(l1o$pred, l1o$true),
                                    perf(bs10$pred, bs10$true),
                                    perf(bs50$pred, bs50$true),
                                    wperf(bs10.632$pred, bs10.632$true, bs10.632$w),
                                    wperf(bs50.632$pred, bs50.632$true, bs50.632$w)))
  }

  names(performance) <- c("true", "3-CV", "5-CV", "10-CV", "20-CV", "4x5-CV",
                          "HO", "10xHO", "L1O", "10-BS", "50-BS",
                          "10-BS.632", "50-BS.632")
  bias <- apply(performance[,-1]-performance[,1], 2, mean)
  variance <- apply(performance[,-1], 2, var)

  list(performance=performance, bias=bias, variance=variance)
}

  # the commented lines run a 200-repetition experiment, which takes a long time
#s01.ebv <- eval.bias.var(rpart, Class~., Soybean01,
#                         predf=function(...) predict(..., type="c"), n=200)
  # this can be used for a quick illustration
s01.ebv <- eval.bias.var(rpart, Class~., Soybean01,
                         predf=function(...) predict(..., type="c"), n=10)

boxplot(s01.ebv$performance[,-1], main="Error", las=2)
lines(c(0, 13), rep(mean(s01.ebv$performance[,1]), 2))
barplot(s01.ebv$bias, main="Bias", las=2)
barplot(s01.ebv$variance, main="Variance", las=2)
