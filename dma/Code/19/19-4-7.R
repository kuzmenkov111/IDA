## random forest filter
rf.filter <- function(formula, data, ...)
{
  if (require(randomForest, quietly=TRUE))
  {
    rf <- randomForest(formula, na.roughfix(data), importance=TRUE, ...)
    sort(importance(rf, type=1)[,1], decreasing=TRUE)

  }
  else
  {
    attributes <- x.vars(formula, data)
    names<-(rep(1, length(attributes), attributes))
  }
}

  # random forest filter for the weather data
rf.filter(play~., weather)
  # random forest filter for the weatherc data
rf.filter(play~., weatherc)
  # random forest filter for the weatherr data
rf.filter(playability~., weatherr)

  # random forest filter for the Vehicle Silhouettes data
v.utl.rf <- rf.filter(Class~., v.train)
  # random forest filter for the Soybean data
s.utl.rf <- rf.filter(Class~., s.train)
  # random forest filter for the BostonHousing data
bh.utl.rf <- rf.filter(medv~., bh.train)
