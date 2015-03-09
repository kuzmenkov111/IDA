## filter-driven attribute selection search
asel.search.filter <- function(attributes, target, utils, evalf, penalty=0.01)
{
  ev <- function(subset)
  {
    ifelse(is.finite(v<-evalf(subset, target)), v-penalty*length(subset)*abs(v), v)
  }

  subsets <- unname(lapply(utils, function(u) names(utils)[utils>=u]))
  subsets.eval <- sapply(subsets, ev)
  s.best <- which.max(subsets.eval)

  list(subset=subsets[[s.best]], eval=subsets.eval[s.best])
}

  # filter attribute selection search for the weather data
  # using mutual information-based utility estimates
asel.search.filter(names(weather)[-5], "play",
                   simple.filter(play~., weather, dd=symunc),
                   evalf=function(subset, target) 1)
asel.search.filter(names(weather)[-5], "play",
                   simple.filter(play~., weather, dd=symunc),
                   evalf=function(subset, target) 1, penalty=0)
asel.search.filter(names(weather)[-5], "play",
                   simple.filter(play~., weather, dd=symunc),
                   evalf=function(subset, target) 1)
asel.search.filter(names(weather)[-5], "play",
                   simple.filter(play~., weather, dd=symunc),
                   evalf=function(subset, target) 1, penalty=0)
