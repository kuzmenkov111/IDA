# http://berndbischl.github.io/mlr/tutorial/html/train/index.html

library(mlr)

## function signature
#train(learner, task, subset, weights = NULL)

## survival analysis
data(lung, package="survival")
lung$status = (lung$status == 2)

task = makeSurvTask(data=lung, target=c("time","status"))
lrn = makeLearner("surv.coxph")

mod = train(lrn, task)
mod

# creating a learner explicitly is not strictly necessary
mod = train("surv.coxph", task)
mod

# recommended if not default hyperparameters, prediction type ...

## cluster analysis
task = makeClusterTask(data=mtcars)
lrn = makeLearner("cluster.kmeans", centers=3)

mod = train(lrn, task)
mod

## only a subset of data, specified by an index set, can be used to train the learner
task = bh.task
n = task$task.desc$size
trainSet = sample(n, size=n/3)

mod = train("regr.lm", task, subset=trainSet)
mod

# all standard resample strategies are supported - no manual sampling necessary

## observation weight available if a learner supports
target = getTaskTargets(bc.task)
tab = as.numeric(table(target))
w = 1/tab[target]

train("classif.rpart", task=bc.task, weights=w)

# weights set in make tasks is overwritten

## wrapped models - actual fitted model + additional info about learner and task
mod = train("regr.lm", bh.task, subset=sample(bh.task$task.desc$size, bh.task$task.desc$size/3))
getLearnerModel(mod)
