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
