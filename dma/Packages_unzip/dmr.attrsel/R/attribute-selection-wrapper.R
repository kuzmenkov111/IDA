## wrapper attribute selection
wrapper.select <- function(formula, data, alg, args=NULL, predf=predict,
                           searchf=asel.search.greedy,
                           initf=asel.init.all, nextf=asel.next.backward,
                           perf=if (is.numeric(data[[target]])) mse else err, ...)
{
  target <- y.var(formula)
  attributes <- x.vars(formula, data)
  searchf(attributes, target,
          evalf=function(subset, target)
                wrapper.eval(subset, target, data, alg, args, predf, perf, ...),
          initf=initf, nextf=nextf)
}


if (FALSE)
{

  # wrapper selection for the weather data
wrapper.select(play~., weather, rpart, args=list(minsplit=2),
               predf=function(...) predict(..., type="c"))
wrapper.select(play~., weather, rpart, args=list(minsplit=2),
               predf=function(...) predict(..., type="c"),
               initf=asel.init.none, nextf=asel.next.forward)

  # wrapper selection for the weatherc data
wrapper.select(play~., weatherc, rpart, args=list(minsplit=2),
               predf=function(...) predict(..., type="c"))
wrapper.select(play~., weatherc, rpart, args=list(minsplit=2),
               predf=function(...) predict(..., type="c"),
               initf=asel.init.none, nextf=asel.next.forward)

  # wrapper selection for the weatherr data
wrapper.select(playability~., weatherr, rpart, args=list(minsplit=2))
wrapper.select(playability~., weatherr, rpart, args=list(minsplit=2),
               initf=asel.init.none, nextf=asel.next.forward)

  # wrapper selection for the Vehicle Silhouettes data
v.sel.fwd <- wrapper.select(Class~., v.train, rpart,
                            predf=function(...) predict(..., type="c"),
                            initf=asel.init.none, nextf=asel.next.forward)
v.sel.bwd <- wrapper.select(Class~., v.train, rpart,
                            predf=function(...) predict(..., type="c"))

  # wrapper selection for the Soybean data
s.sel.fwd <- wrapper.select(Class~., s.train, rpart,
                            predf=function(...) predict(..., type="c"),
                            initf=asel.init.none, nextf=asel.next.forward)
s.sel.bwd <- wrapper.select(Class~., s.train, rpart,
                            predf=function(...) predict(..., type="c"))

  # wrapper selection for the Boston Housing data
bh.sel.fwd <- wrapper.select(medv~., bh.train, rpart,
                             initf=asel.init.none, nextf=asel.next.forward)
bh.sel.bwd <- wrapper.select(medv~., bh.train, rpart)

}
