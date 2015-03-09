  # leave-one-out regression tree evaluation for the BostonHousing data
bh.l1o <- leave1out(rpart, medv~., BostonHousing)
mse(bh.l1o$pred, bh.l1o$true)
