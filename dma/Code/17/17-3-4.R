## single-attribute mean/median/mode imputation transformation
imp <- function(v, med=FALSE)
{
  if (!is.numeric(v))
    modal(v)
  else if (med)
    median(v, na.rm=TRUE)
  else
    mean(v, na.rm=TRUE)
}

## imputation for all attributes
imp.all <- transmod.all(imp)

## imputation model prediction
predict.imp <- predict.transmod(function(m, v) { v[is.na(v)] <- m; v } )

weathercm <- weatherc
weathercm$outlook[c(1, 3)] <- NA
weathercm$temperature[c(2, 4)] <- NA
weathercm$humidity[c(3, 5)] <- NA
weathercm$wind[c(4, 6)] <- NA

gm.train <- g.train
gm.train[sample.int(nrow(gm.train), 0.1*nrow(gm.train)),
         sample.int(ncol(gm.train)-1, 3)] <- NA
gm.test <- g.test
gm.test[sample.int(nrow(gm.test), 0.1*nrow(gm.test)),
        sample.int(ncol(gm.test)-1, 3)] <- NA

  # imputation model for the weatherc data
wm.impm <- imp.all(play~., weathercm, med=TRUE)
  # applied to the weatherc data
wm.imp <- predict.imp(wm.impm, weathercm)

  # imputation model for the Glass data
gm.impm <- imp.all(Type~., gm.train)
  # applied to the training and test sets
gm.train.imp <- predict.imp(gm.impm, gm.train)
gm.test.imp <- predict.imp(gm.impm, gm.test)
