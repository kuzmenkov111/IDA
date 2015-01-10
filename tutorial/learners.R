library(mlr)

## constructing a learner
# select class of learner (cl)
# set hyperparameters (par.vals)
# control output or prediction type (predict.type)
# set id (default: id=cl)

## specify hyperparameters via a list
regrLearner = makeLearner(cl="regr.gbm", par.vals=list(n.trees=500, interaction.depth=3))

## set prediction type
classifLearner = makeLearner("classif.randomForest", predict.type="prob", fix.factors=TRUE)

## set id
survLearner = makeLearner("surv.coxph", id="cph")

## ... parameters

clusterLearner = makeLearner("cluster.SimpleKMeans", N=5)

