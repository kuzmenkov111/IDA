## greedy attribute selection search
asel.search.greedy <- function(attributes, target, evalf,
                               initf=asel.init.all, nextf=asel.next.backward,
                               max.noimp=3, penalty=0.01)
{
  ev <- function(subset)
  {
    ifelse(is.finite(v<-evalf(subset, target)), v-penalty*length(subset)*abs(v), v)
  }

  best.subset <- subset <- initf(attributes)
  best.eval <- eval <- ev(subset)
  noimp <- 0

  while (noimp < max.noimp)
  {
    candidates <- nextf(subset, attributes)
    if (length(candidates)>0)
    {
      cand.eval <- sapply(candidates, ev)
      cand.best <- which.max(cand.eval)
      noimp <- ifelse(cand.eval[cand.best]>eval, 0, noimp+1)
      subset <- candidates[[cand.best]]
      eval <- cand.eval[cand.best]
      if (eval>best.eval)
      {
        best.subset <- subset
        best.eval <- eval
      }
    }
    else
      break
  }

  list(subset=best.subset, eval=best.eval)
}


if (FALSE)
{

  # greedy attribute selection search for the weather data
asel.search.greedy(names(weather)[-5], "play", evalf=function(subset, target) 1)
asel.search.greedy(names(weather)[-5], "play", evalf=function(subset, target) 1,
                   penalty=0)
asel.search.greedy(names(weather)[-5], "play", evalf=function(subset, target) 1,
                   initf=asel.init.none, nextf=asel.next.forward)
asel.search.greedy(names(weather)[-5], "play", evalf=function(subset, target) 1,
                   initf=asel.init.none, nextf=asel.next.forward, penalty=0)

}
