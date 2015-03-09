library(dmr.claseval)
library(dmr.util)

library(rpart)
library(e1071)

data(weatherc, package="dmr.data")
data(Vehicle, package="mlbench")
data(Glass, package="mlbench")

set.seed(12)

rv <- runif(nrow(Vehicle))
v.train <- Vehicle[rv>=0.33,]
v.test <- Vehicle[rv<0.33,]

rg <- runif(nrow(Glass))
g.train <- Glass[rg>=0.33,]
g.test <- Glass[rg<0.33,]

  # baseline models and their prediction
v.tree <- rpart(Class~., v.train)
v.tree.pred <- predict(v.tree, v.test, type="c")

g.tree <- rpart(Type~., g.train)
g.tree.pred <- predict(g.tree, g.test, type="c")

v.nb <- naiveBayes(Class~., v.train)
v.nb.pred <- predict(v.nb, v.test, type="c")

g.nb <- naiveBayes(Type~., g.train)
g.nb.pred <- predict(g.nb, g.test, type="c")
