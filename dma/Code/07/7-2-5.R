s01.labels <- c("other", "brown-spot")
Soybean01 <- Soybean
Soybean01$Class <- factor(ifelse(Soybean$Class=="brown-spot", "brown-spot", "other"),
                          levels=s01.labels)

s01.train <- Soybean01[rs>=0.33,]
s01.test <- Soybean01[rs<0.33,]

s01.tree <- rpart(Class~., s01.train)
