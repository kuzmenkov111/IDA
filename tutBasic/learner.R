# http://berndbischl.github.io/mlr/tutorial/html/learner/index.html

library(mlr)

## function signature
#makeLearner(cl, id = cl, predict.type = "response", fix.factors = FALSE,
#..., par.vals = list(), config = list())

## constructing a learner
# select class of learner (cl)
# set hyperparameters (par.vals or ...)
# control output or prediction type (predict.type)
# set id (default: id=cl)
# Occasionally, factor features may cause problems when fewer levels are present in the test data than in the training data. By setting fix.factors = TRUE these are avoided by adding a factor level for missing data in the test data set.

## specify hyperparameters via a list
regrLearner = makeLearner(cl="regr.gbm", par.vals=list(n.trees=500, interaction.depth=3))

## set prediction type
classifLearner = makeLearner("classif.randomForest", predict.type="prob", fix.factors=TRUE)

## ... parameters
clusterLearner = makeLearner("cluster.SimpleKMeans", N=5)

## set id
survLearner = makeLearner("surv.coxph", id="cph")

# no special learner class for cost-sensitive classification

## accessing a learner
# get the selected hyperparameter values
clusterLearner$par.vals
getHyperPars(clusterLearner)

# get the set of hyperparameters
classifLearner$par.set
getParamSet(classifLearner)

# get the type of prediction
regrLearner$predict.type

# see overview of available hyperparameters and defaults of a learning method before actually creating one.
showHyperPars("classif.randomForest")

## modifying a learner
survLearner = setId(survLearner, "CoxModel")

# change prediction type
classifLearner = setPredictType(classifLearner, "response")

# change hyper parameter values
clusterLearner = setHyperPars(clusterLearner, N=4)

# go back to default hyperparameter values
regrLearner = removeHyperPars(regrLearner, c("n.trees","interaction.depth"))



