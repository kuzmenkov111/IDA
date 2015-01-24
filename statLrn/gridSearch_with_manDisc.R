library(ggplot2)
library(mlr)
# set parameters for search
ps = makeParamSet(makeDiscreteParam("C", values=2^(-2:2)),
                  makeDiscreteParam("sigma", values=2^(-2:2)))

# create tune control object, grid search for now
ctrl = makeTuneControlGrid()

# set resampling description
rdesc = makeResampleDesc("CV", iters=3L)

# turn parameters
set.seed(123457)
mmce.res = tuneParams("classif.ksvm", task=iris.task, resampling=rdesc, par.set=ps, control=ctrl, show.info=FALSE)
mmce.res
mmce.res$opt.path
mmce.opt.grid = as.data.frame(mmce.res$opt.path)

acc.res = tuneParams("classif.ksvm", task=iris.task, resampling=rdesc, par.set=ps, control=ctrl
                     , measures=list(acc, setAggregation(acc, test.sd)), show.info=FALSE)
acc.res
acc.res$opt.path
acc.opt.grid = as.data.frame(acc.res$opt.path)

g = ggplot(acc.opt.grid, aes(x=C, y=sigma, fill=acc.test.sd, label=round(acc.test.mean,3)))
g + geom_tile() + geom_text(color="white")

# using optimal parameter values
lrn = setHyperPars(makeLearner("classif.ksvm"), par.vals=acc.res$x)
lrn

mod = train(lrn, iris.task)
predict(mod, task=iris.task)