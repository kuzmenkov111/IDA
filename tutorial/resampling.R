# http://berndbischl.github.io/mlr/tutorial/html/resample/index.html

library(mlr)

## function signiture
# specify resampling strategy
# makeResampleDesc(method, predict = "test", ..., stratify = FALSE)
# predict = "test" | "train" | "both"

# calculate performance
#resample(learner, task, resampling, measures, weights = NULL,
#         models = FALSE, extract, show.info = getMlrOption("show.info"))

# cross-validation ("CV")
# leave-one-out cross-validation ("LOO")
# repeated cross-validation ("RepCV")
# out-of-bag bootstrap("Bootstrap")
# subsampling("Subsample")
# holdout (training/test) ("Holdout")

## example - 3-fold cross-validation
rdesc = makeResampleDesc("CV", iters=3)
r = resample("surv.coxph", lung.task, rdesc, measures=list(cindex,timetrain))
r$aggr
r$measures.test
r$pred
head(r$pred$data)

## example - subsampling with 5 iterations (with 1 iteration is just holdout or test sample estimation)
rdesc = makeResampleDesc("Subsample", iters=5) # split=2/3 as default
rdesc = makeResampleDesc("Subsample", iters=5, split=4/5)

lrn = makeLearner("cluster.kmeans", centers=3)
r = resample(lrn, mtcars.task, rdesc, measures = timetrain)

## stratified resampling
rdesc = makeResampleDesc("CV", iters=3, stratify=TRUE)
r = resample("classif.lda", iris.task, rdesc)





