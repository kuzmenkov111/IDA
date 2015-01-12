# http://berndbischl.github.io/mlr/tutorial/html/performance/index.html

library(mlr)

## function signiture
# performance(pred, measures, task = NULL, model = NULL, feats = NULL)

## Implemented Performance Measures
# http://berndbischl.github.io/mlr/tutorial/html/measures/index.html

## classification
# mean misclassification error (mmce)
# accuracy (acc)
# ROC analysis
## regression
# mean squared error (mse)
# mean absolute error (mae)
## clustering
# Dunn index (dunn)
## survival analysis
# concordance index (cindex)
## cost sensitive classification
# misclassification penalty (mcp)
## general performance measures
# time to train the learner (timetrain)
# time to compute the prediction (timepredict)
# timetrain + time predict = timeboth

## find a suitable performance measure
listMeasures("classif", properties="classif.multi")
listMeasures(iris.task)
listMeasures(bh.task)

## calculate performance measures
task = bh.task
n = task$task.desc$size
lrn = makeLearner("regr.gbm", n.trees=1000)
mod = train(lrn, task=task, subset=seq(1, n, 2))
pred = predict(mod, task=task, subset=seq(2, n, 2))

performance(pred) # default: mse for regression
performance(pred, measures=medse)
performance(pred, measures=list(mse, medse, mae))

## requirements of performance measures
# some perfromance measures require task or fitted model together with prediction
performance(pred, measures=timetrain, model=mod)

# task is required for many performance measures in cluster analysis
lrn = makeLearner("cluster.kmeans", centers=3)
mod = train(lrn, mtcars.task)
pred = predict(mod, task=mtcars.task)

performance(pred, measures=dunn, task=mtcars.task)

# some measures require a certain type of prediction
# eg in binary classification, AUC (area under ROC), predict.type should be prob
lrn = makeLearner("classif.rpart", predict.type="prob")
mod = train(lrn, task=sonar.task)
pred = predict(mod, task=sonar.task)

performance(pred, measures=auc)

# many performance measures (eg false positive rate (pr)) suitable only for binary problems

## access a performance measure(, which is an object of class Measure)
str(mmce)

## binary classification: plot performance vs threshold
lrn = makeLearner("classif.lda", predict.type="prob")
n = sonar.task$task.desc$size
mod = train(lrn, task=sonar.task, subset=seq(1, n, by=2))
pred = predict(mod, task=sonar.task, subset=seq(2, n, by=2))

performance(pred, measures=list(fpr, fnr, mmce))

plotThreshVsPerf(pred, measures=list(fpr, fnr, mmce))























