## single-attribute aggregation transformation with m most frequent values retained
## and others replaced by comb.val
agg <- function(v, m, comb.val="other")
{
  list(retained=names(sort(table(v), decreasing=TRUE))[1:min(m, nlevels(v))],
       combined=comb.val)
}

## normalization of all discrete attributes
agg.all <- transmod.all(agg, is.factor)

## aggregation model prediction
predict.agg <- predict.transmod(function(m, v)
                                factor(ifelse(v %in% m$retained,
                                              as.character(v),
                                              ifelse(is.na(v), NA, m$combined)),
                                       levels=c(m$retained, m$combined)))

  # aggregation model for the weatherc data
w.aggm <- agg.all(play~., weatherc, 1)
  # applied to the weatherc data
w.agg <- predict.agg(w.aggm, weatherc)
