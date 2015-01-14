# http://berndbischl.github.io/mlr/tutorial/html/benchmark_experiments/index.html

library(mlr)

## function signiture
# benchmark(learners, tasks, resamplings, measures,
#          show.info = getMlrOption("show.info"))

## two tasks and two learners
tasks = list(iris.task, sonar.task)
lrns = list(makeLearner("classif.lda"), makeLearner("classif.rpart"))
rdesc = makeResampleDesc("CV", iters=5)

res = benchmark(lrns, tasks, rdesc, show.info=FALSE)
res

# individual performances of the 5 cross-validation runs
getBMRPerformances(res)

# aggregated performances
getBMRAggrPerformances(res)

# predictions
pred = getBMRPredictions(res)
names(pred)
head(pred$`iris-example`)

# check test and learner ids
getBMRTaskIds(res)
getBMRLearnerIds(res)

## nested resampling
# eg double cross-validation
# 'outer' cv
#   - data is split repeatedly into a (larger) training set and a (smaller) test set
# 'inner' cv
#   - in every outer iteration, learner is tuned on the training set by 'inner' cv
#   - best hyperparameters are selected and used for fitting the learner for complete 'outer' training set
# resulting model is used to on the 'outer' test set
# usage
#   - estimate locations (eg mean or median performance value)
#   - compare learning methods in a fair way

## one task, one learner, tuning
# range of hyperparameter k
ps = makeParamSet(makeDiscreteParam("k", 1:5))
ctrl = makeTuneControlGrid()

# define 'inner' cross-validation indices
in.rdesc = makeResampleDesc("CV", iters=3)

# tune k-nearest neighbor
lrn = makeTuneWrapper("classif.kknn", resampling=in.rdesc
                      ,par.set=ps, control=ctrl, show.info=FALSE)

# define 'outer' bootstrap indices
out.rdesc = makeResampleDesc("Bootstrap", iters=5)

# merge it into a benchmark experiment
# choose accuracy instead of default measure of mmse
res = benchmark(lrn, iris.task, out.rdesc, measure=acc, show.info=FALSE)
res

# What parameter setting achieved this performances?
getBMRTuneResults(res)

# What performances did we get in the single runs?
getBMRPerformances(res)

## two tasks, three learners, tuning
# list of learning tasks
tasks = list(iris.task, sonar.task)

# very small grid for SVM hyperparameters
ps = makeParamSet(makeDiscreteParam("C", 2^seq(-1,1)), makeDiscreteParam("sigma", 2^seq(-1,1)))
ctrl = makeTuneControlGrid()

# define 'inner' cross-validation indicies
in.rdesc = makeResampleDesc("CV", iters=3)

# tune SVM
lrn = makeTuneWrapper("classif.ksvm", resampling=in.rdesc, par.set=ps, control=ctrl, show.info=FALSE)

# three learners to be compared
lrns = list(makeLearner("classif.lda"), makeLearner("classif.rpart"), lrn)

# define 'outer' cross-validation indicies
out.rdesc = makeResampleDesc("CV", iters=5)

# merge it to a benchmark experiment
res = benchmark(lrns, tasks, out.rdesc, show.info=FALSE)
res

# performance values in individual runs
getBMRPerformances(res)

# tuned parameter for SVM
getBMRTuneResults(res)

## one task, two learners, feature selection
# control object for feature selection
ctrl = makeFeatSelControlSequential(beta=100, method="sfs")

# inner resampling
in.rdesc = makeResampleDesc("CV", iter=2)

# feature selection with sequential forward search (sfs)
lrn = makeFeatSelWrapper("classif.lda", resampling=in.rdesc, control=ctrl, show.info=FALSE)

# compare two learners
lrns = list(makeLearner("classif.rpart"), lrn)

# define outer resampling
out.rdesc = makeResampleDesc("Subsample", iter=3)

# benchmark experiment
res = benchmark(tasks=iris.task, learners=lrns, resampling=out.rdesc, show.info=FALSE)
res

# which features have been selected (in the outer resampling steps)?
getBMRFeatSelResults(res)

## performances on individual test data sets
getBMRPerformances(res)