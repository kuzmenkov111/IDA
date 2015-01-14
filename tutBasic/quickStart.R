# http://berndbischl.github.io/mlr/tutorial/html/index.html

library(mlr)
data(iris)

## defind the task
task = makeClassifTask(id="tutorial", data=iris, target="Species")

## define the learner
lrn = makeLearner("classif.lda")

## define the resampling strategy
rdesc = makeResampleDesc(method="CV", stratify=TRUE)

## do resampling
r = resample(task=task, learner=lrn, resampling=rdesc, show.info=FALSE)
