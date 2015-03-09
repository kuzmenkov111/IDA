## cutoff based on decreasingly sorted named attribute utilities
cutoff <- function(utils, k=NA, p=NA)
{
  k <- ifelse(is.na(k), round(p*length(utils)), k)
  k <- ifelse(is.na(k), which.max(-diff(utils)), k)
  k <- ifelse(is.na(k), 1, k)

  names(utils)[1:min(k, length(utils))]
}

  # cutoff based on the random forest filter for the weather data
cutoff(rf.filter(play~., weather), k=3)
  # cutoff based on the random forest filter for the weatherc data
cutoff(rf.filter(play~., weatherc), p=0.5)
  # cutoff based on the random forest filter for the weatherr data
cutoff(rf.filter(playability~., weatherr))

  # cutoff for the Vehicle Silhouettes data
v.sel.simple <- cutoff(v.utl.simple, p=0.5)
v.sel.rf <- cutoff(v.utl.rf, p=0.5)
v.sel.rel <- cutoff(v.utl.rel, p=0.5)

  # cutoff for the Soybean data
s.sel.simple <- cutoff(s.utl.simple, p=0.5)
s.sel.rf <- cutoff(s.utl.rf, p=0.5)
s.sel.rel <- cutoff(s.utl.rel, p=0.5)

  # cutoff for the Boston Housing data
bh.sel.simple <- cutoff(bh.utl.simple, p=0.5)
bh.sel.rf <- cutoff(bh.utl.rf, p=0.5)
bh.sel.rel <- cutoff(bh.utl.rel, p=0.5)
