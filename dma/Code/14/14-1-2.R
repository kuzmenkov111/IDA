## prediction for pam clustering models (only if created with stand=FALSE)
## using daisy or dist (selected via the dmf argument) for dissimilarity calculation
predict.pam <- function(model, data, dmf=daisy, ...)
{
  k.centers.assign(model$medoids, data,
                   function(x1, x2) dmf(rbind(x1, x2), ...))
}

## the same is applicable to clara clustering models
predict.clara <- predict.pam

  # test set predictions
i.pam2.euc.pred <- predict(i.pam2.euc, i.std.test[,-5])
i.pam3.euc.pred <- predict(i.pam3.euc, i.std.test[,-5])
i.pam5.euc.pred <- predict(i.pam5.euc, i.std.test[,-5])
i.pam7.euc.pred <- predict(i.pam7.euc, i.std.test[,-5])

i.pam2.man.pred <- predict(i.pam2.man, i.std.test[,-5])
i.pam3.man.pred <- predict(i.pam3.man, i.std.test[,-5])
i.pam5.man.pred <- predict(i.pam5.man, i.std.test[,-5])
i.pam7.man.pred <- predict(i.pam7.man, i.std.test[,-5])
