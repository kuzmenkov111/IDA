library(dmr.util)
library(rpart)

data(Soybean, package="mlbench")

set.seed(12)
rs <- runif(nrow(Soybean))
s.train <- Soybean[rs>=0.33,]
s.test <- Soybean[rs<0.33,]

s.tree <- rpart(Class~., s.train)
