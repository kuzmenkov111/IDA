## single-attribute standardization transformation
std <- function(v)
{
  list(mean=mean(v, na.rm=TRUE), sd=sd(v, na.rm=TRUE))
}

## standardization of all continuous attributes
std.all <- transmod.all(std, is.numeric)

## standardization model prediction
predict.std <- predict.transmod(function(m, v) (v-m$mean)/m$sd)


if (FALSE)
{

  # standardization model for the weatherc data
w.stdm <- std.all(play~., weatherc)
  # applied to the weatherc data
w.std <- predict.std(w.stdm, weatherc)

  # standardization model for the Glass data
g.stdm <- std.all(Type~., g.train)
  # applied to the training and test sets
g.train.std <- predict.std(g.stdm, g.train)
g.test.std <- predict.std(g.stdm, g.test)

}
