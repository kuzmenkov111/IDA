## single-attribute normalization transformation
nrm <- function(v)
{
  list(min=min(v, na.rm=TRUE), max=max(v, na.rm=TRUE))
}

## normalization of all continuous attributes
nrm.all <- transmod.all(nrm, is.numeric)

## standardization model prediction
predict.nrm <- predict.transmod(function(m, v) (v-m$min)/(m$max-m$min))


if (FALSE)
{

  # normalization model for the weatherc data
w.nrmm <- nrm.all(play~., weatherc)
  # applied to the weatherc data
w.nrm <- predict.nrm(w.nrmm, weatherc)

  # normalization model for the Glass data
g.nrmm <- nrm.all(Type~., g.train)
  # applied to the training and test sets
g.train.nrm <- predict.nrm(g.nrmm, g.train)
g.test.nrm <- predict.nrm(g.nrmm, g.test)

}
