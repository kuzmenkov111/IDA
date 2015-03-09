library(dmr.claseval)
library(dmr.util)

library(rpart)
library(e1071)
library(ipred)

data(Vehicle, package="mlbench")

set.seed(12)
rv <- runif(nrow(Vehicle))
v.train <- Vehicle[rv>=0.33,]
v.test <- Vehicle[rv<0.33,]

  # two-class version
Vehicle01 <- Vehicle
Vehicle01$Class <- factor(ifelse(Vehicle$Class %in% c("opel", "saab"),
                                 "car", "other"))
v01.train <- Vehicle01[rv>=0.33,]
v01.test <- Vehicle01[rv<0.33,]

  # cost-insensitive decision trees
v.tree <- rpart(Class~., v.train)
v01.tree <- rpart(Class~., v01.train)

  # cost-insensitive naive Bayes classifiers
v.nb <- naiveBayes(Class~., v.train)
v01.nb <- naiveBayes(Class~., v01.train)

  # misclassification error for cost-insensitive models
v.err.b <- list(tree=err(predict(v.tree, v.test, type="c"), v.test$Class),
                nb=err(predict(v.nb, v.test), v.test$Class))

v01.err.b <- list(tree=err(predict(v01.tree, v01.test, type="c"), v01.test$Class),
                  nb=err(predict(v01.nb, v01.test), v01.test$Class))
