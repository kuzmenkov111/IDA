## filter-driven wrapper attribute selection
wrapper.filter.select <- function(formula, data, utils, alg, args=NULL,
                                  predf=predict, searchf=asel.search.filter,
                                  perf=if (is.numeric(data[[target]])) mse else err,
                                  ...)
{
  target <- y.var(formula)
  attributes <- x.vars(formula, data)
  searchf(attributes, target, utils,
          evalf=function(subset, target)
                wrapper.eval(subset, target, data, alg, args, predf, perf, ...))
}


if (FALSE)
{

  # simple filter-driven wrapper selection for the weather data
wrapper.filter.select(play~., weather, simple.filter(play~., weather),
                      rpart, args=list(minsplit=2),
                      predf=function(...) predict(..., type="c"))

  # simple filter-driven wrapper selection for the weatherc data
wrapper.filter.select(play~., weatherc, simple.filter(play~., weatherc),
                      rpart, args=list(minsplit=2),
                      predf=function(...) predict(..., type="c"))

  # simple filter-driven wrapper selection for the weatherr data
wrapper.filter.select(playability~., weatherr,
                      simple.filter(playability~., weatherr),
                      rpart, args=list(minsplit=2))

  # RF filter-driven wrapper selection for the Vehicle Silhouettes data
v.sel.flt <- wrapper.filter.select(Class~., Vehicle, rf.filter(Class~., v.train),
                                   rpart, predf=function(...) predict(..., type="c"))

  # RF filter-driven wrapper selection for the Soybean data
s.sel.flt <- wrapper.filter.select(Class~., Soybean, rf.filter(Class~., s.train),
                                   rpart, predf=function(...) predict(..., type="c"))

  # RF filter-driven wrapper selection for the Boston Housing data
bh.sel.flt <- wrapper.filter.select(medv~., BostonHousing,
                                    rf.filter(medv~., bh.train), rpart)

}
