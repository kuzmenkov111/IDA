# http://berndbischl.github.io/mlr/tutorial/html/predict/index.html

library(mlr)

## function signature
# predict (object, ...)

## ways to pass data for prediction - 'task' and 'newdata'
# pass via 'task'
n = bh.task$task.desc$size
bhTrain = seq(1, n, by=2)
bhTest = seq(2, n, by=2)
bhLearn = makeLearner("regr.gbm", n.trees=100)
bhMod = train(bhLearn, bh.task, subset=bhTrain)

bhPred = predict(bhMod, task=bh.task, subset=bhTest)
bhPred

# accessing prediction
head(bhPred$data)

# when predicting from a taks, additional column 'id' created, which is row number of original data

# pass via 'newdata'
n = nrow(iris)
irisTrain = iris[seq(1, n, by = 2), -5]
irisTest = iris[seq(2, n, by = 2), -5]
irisTask = makeClusterTask(data = irisTrain)
irisMod = train("cluster.kmeans", irisTask)

irisPred = predict(irisMod, newdata = irisTest)
irisPred

# accessing prediction
head(irisPred$data)

## extract probabilities
# cluster analysis
mtLearn = makeLearner("cluster.cmeans", predict.type="prob")
mtMod = train(mtLearn, mtcars.task)

mtPred = predict(mtMod, task=mtcars.task)
head(getProbabilities(mtPred))

# classification
# class labels are predicted by default - appropriate 'predict.type' should be set in a learner
irisMod = train("classif.lda", task=iris.task)

irisPred = predict(irisMod, task = iris.task)
irisPred

# confusion matrix
getConfMatrix(irisPred)

# set 'predict.type="prob"'
irisLearn = makeLearner("classif.rpart", predict.type="prob")
irisMod = train(irisLearn, iris.task)

irisPred = predict(irisMod, newdata=iris)
head(irisPred$data)

# In addition to the probabilities, class labels are predicted by choosing the class with the maximum probability and breaking ties at random

head(getProbabilities(irisPred))

## adjusting threshold
# binary classification
# need to create a learner that predicts probabilities
sonarLearner = makeLearner("classif.rpart", predict.type="prob")
sonarMod = train(sonarLearner, task=sonar.task)

# label of positive class
sonar.task$task.desc$positive

# default threshold
sonarPred = predict(sonarMod, sonar.task)
sonarPred$threshold
sonarPred

# set the threshold value for positive class
sonarPred = setThreshold(sonarPred, 0.8)
sonarPred$threshold
sonarPred

# in binary case, getProbabilities extracts posterior probabilities of positive class only
head(getProbabilities(sonarPred))

# multiclass classification
irisLearn = makeLearner("classif.rpart", predict.type="prob")
irisMod = train(irisLearn, iris.task)
irisPred = predict(irisMod, newdata=iris)

irisPred$threshold
table(as.data.frame(irisPred)$response)

irisPred = setThreshold(irisPred, c(setosa=0.01, versicolor=50, virginica=1))
irisPred$threshold
table(as.data.frame(irisPred)$response)

## visualizing prediction
# 2d for classification
irisLearn = makeLearner("classif.rpart", id="CART")
plotLearnerPrediction(irisLearn, task=iris.task)

# 1d for regression
plotLearnerPrediction("regr.lm", features="lstat", task=bh.task)

# 2d for regression
plotLearnerPrediction("regr.lm", features=c("lstat", "rm"), task=bh.task)







