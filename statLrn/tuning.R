library(mlr)

## grid search with manual discretization
# parameter values have to be determined
set.seed(1)
ps.w = makeParamSet(makeDiscreteParam("C", values=2^(-2:2)),
                    makeDiscreteParam("sigma", values=2^(-2:2)))
ctrl.w = makeTuneControlGrid()
rdesc.w = makeResampleDesc("CV", iters=3L)
# default measure - mean misclassification error (mmce)
# can be modified eg measures=setAggregate(acc, test.sd) -> report both, tuned by fist (acc)
res.w = tuneParams("classif.ksvm", task=iris.task, resampling=rdesc.w, par.set=ps.w, control=ctrl.w, show.info=FALSE)
res.w
res.w.opt.grid <- as.data.frame(res.w$opt.path)
# optimal parameters
res.w$x
# measure at optimal parameters
res.w$y

# optimal parameters can be reused
lrn.w = setHyperPars(makeLearner("classif.ksvm", par.vals=res.w$x))
lrn.w

mod.w = train(lrn.w, iris.task)
predict(mod.w, task=iris.task)

## grid search without manual discretization
# parameters can be entered more flexibly by transformation (trafo) and by setting intervals (resolution)
set.seed(1)
trans <- function(x) 2^x
ps.wo = makeParamSet(makeNumericParam("C", lower=-2, upper=2, trafo=trans),
                     makeNumericParam("sigma", lower=-2, upper=2, trafo=trans))
ctrl.wo=makeTuneControlGrid(resolution=5L) # interval set by resolution
rdesc.wo = makeResampleDesc("CV", iters=3L)
res.wo = tuneParams("classif.ksvm", iris.task, rdesc.wo, par.set=ps.wo, control=ctrl.wo, show.info=FALSE)
res.wo
res.wo.opt.grid <- as.data.frame(res.wo$opt.path)
res.wo.opt.grid$C <- trans(res.wo.opt.grid$C)
res.wo.opt.grid$sigma <- trans(res.wo.opt.grid$sigma)

## nested resampling
# tuning in inner resampling and model selection in outer resampling
set.seed(1)
ps.n = makeParamSet(makeDiscreteParam("C", values=2^(-2:2)),
                    makeDiscreteParam("sigma", values=2^(-2:2)))
ctrl.n = makeTuneControlGrid()
rdesc.n.inner = makeResampleDesc("Holdout")
rdesc.n.outer = makeResampleDesc("CV", iters=3L)
lrn.n = makeTuneWrapper("classif.ksvm", rdesc.n.inner, par.set=ps.n, control=ctrl.n, show.info=FALSE)
resample.n = resample(lrn.n, iris.task, resampling=rdesc.n.outer, extract=getTuneResult, show.info=FALSE)
# mmce at each outer loop
resample.n$measures.test
# aggregated mmce
resample.n$aggr
# it depends on samples, when set.seed(123457), nested resampling shows lower mmce

res.n.opt.grid <- lapply(resample.n$extract, function(x) as.data.frame(x$opt.path))
head(res.n.opt.grid[[1]])








