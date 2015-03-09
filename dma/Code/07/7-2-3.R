mean.cost <- function(pred.y, true.y, rho) { mean(diag(rho[pred.y,true.y])) }

  # uniform cost matrix
s.r1test <- matrix(1, nrow=nlevels(s.test$Class), ncol=nlevels(s.test$Class))
diag(s.r1test) <- 0
mean.cost(predict(s.tree, s.test, type="c"), s.test$Class, s.r1test)

  # double cost for misclassifying the least frequent class
s.r2test <- matrix(1, nrow=nlevels(s.test$Class), ncol=nlevels(s.test$Class))
s.r2test[,levels(s.test$Class)=="herbicide-injury"] <- 2
diag(s.r2test) <- 0
mean.cost(predict(s.tree, s.test, type="c"), s.test$Class, s.r2test)
  # this should give the same result
sum(s.w2test)/nrow(s.test)*werr(predict(s.tree, s.test, type="c"),
                                s.test$Class, s.w2test)

  # random per-class costs 1..5
s.r3test <- matrix(s.wctest, nrow=nlevels(s.test$Class), ncol=nlevels(s.test$Class),
                   byrow=TRUE)
diag(s.r3test) <- 0
mean.cost(predict(s.tree, s.test, type="c"), s.test$Class, s.r3test)
  # this should give the same result
sum(s.w3test)/nrow(s.test)*werr(predict(s.tree, s.test, type="c"),
                                s.test$Class, s.w3test)

  # random costs 1..5
s.r4test <- matrix(round(runif(nlevels(s.test$Class)*nlevels(s.test$Class),
                               min=1, max=5)),
                   nrow=nlevels(s.test$Class), ncol=nlevels(s.test$Class))
diag(s.r4test) <- 0
mean.cost(predict(s.tree, s.test, type="c"), s.test$Class, s.r4test)
