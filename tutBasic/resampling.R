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

# stratification on the input columns is not possible

## accessing individual learner models
# to keep wrapped models on each iteration, 'models=TRUE'
rdesc = makeResampleDesc("CV", iters=3)
r = resample("classif.lda", iris.task, rdesc, models=TRUE)
r$models

# or only a part of the model can be extracted
r = resample("regr.rpart", bh.task, rdesc, extract=function(x) x$learner.model$variable.importance)

require(plyr)
varIm <- ldply(.data=r$extract, .fun=rbind)

## resample descriptions and resample instances
# description
rdesc = makeResampleDesc("CV", iters = 3)
str(rdesc)

# instances - can be created by either task or data size
rin = makeResampleInstance(rdesc, task=iris.task)
rin

rin = makeResampleInstance(rdesc, size=nrow(iris))
rin

str(rin)
rin$train.inds[[3]]

# reample instances allow for paired experiments - comparing learners on the same training and test sets
rdesc = makeResampleDesc("CV", iters=3)
rin = makeResampleInstance(rdesc, task=iris.task)

rLDA = resample("classif.lda", iris.task, rin, show.info=FALSE)
rRpart = resample("classif.rpart", iris.task, rin, show.info=FALSE)

rLDA$aggr
rRpart$aggr

# not randomly split
rin = makeFixedHoldoutInstance(train.inds=1:100, test.inds=101:150, size=150)
rin

## aggregating performance values
mmce$aggr # Aggregation function: test.mean

# aggregation function can be changed by setAggregation(measure, aggr)
# mean to sd
mmce.test.sd = setAggregation(mmce, test.sd)
rdesc = makeResampleDesc("CV", iters=3)
r = resample("classif.rpart", iris.task, rdesc, measures=list(mmce, mmce.test.sd))
r$aggr

# training error
mmce.train.mean = setAggregation(mmce, train.mean)
rdesc = makeResampleDesc("CV", iters=3, predict="both")
r = resample("classif.rpart", iris.task, rdesc, measures=list(mmce, mmce.train.mean))
r$measures.train
r$aggr

# bootstrap
rdesc = makeResampleDesc("Bootstrap", predict="both", iters=10)
b632.mmce = setAggregation(mmce, b632)
b632plus.mmce = setAggregation(mmce, b632plus)
b632.mmce

r = resample("classif.rpart", iris.task, rdesc
             ,measures=list(mmce,b632.mmce,b632plus.mmce), show.info=FALSE)
head(r$measures.train)
r$aggr

## convenience functions for frequently used resampling strategies
holdout("regr.lm", bh.task, measures=list(mse,mae))
crossval("classif.lda", iris.task, iters=3, measures=list(mmce,ber))
bootstrapB632("surv.coxph", lung.task, iters=20)