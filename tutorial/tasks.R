# http://berndbischl.github.io/mlr/tutorial/html/task/index.html

library(mlr)

## function signature
#makeClassifTask(id, data, target, weights = NULL, blocking = NULL,
#                positive = NA_character_, fixup.data = "warn", check.data = TRUE)

#makeClusterTask(id, data, weights = NULL, blocking = NULL,
#                fixup.data = "warn", check.data = TRUE)

#makeCostSensTask(id, data, costs, blocking = NULL, fixup.data = "warn",
#                 check.data = TRUE)

#makeRegrTask(id, data, target, weights = NULL, blocking = NULL,
#             fixup.data = "warn", check.data = TRUE)

#makeSurvTask(id, data, target, censoring = "rcens", weights = NULL,
#             blocking = NULL, fixup.data = "warn", check.data = TRUE)


## cluster analysis
data(mtcars, package="datasets")
clusterTask = makeClusterTask(id="mt", data = mtcars)
clusterTask

## regression
data(BostonHousing, package="mlbench")
regrTask = makeRegrTask(id="bh", data=BostonHousing, target="medv")
regrTask

## classification
# target variable has to be a factor
data(BreastCancer, package="mlbench")
BreastCancer$Id = NULL
# some factors are ordered factors - sapply(BreastCancer, is.ordered)
classifTask = makeClassifTask(id="bc", data=BreastCancer, target="Class")
# positive label can be selected
# classifTask = makeClassifTask(id="bc", data=BreastCancer, target="Class", positive="malignant")
classifTask

## survival analysis
data(lung, package="survival")
lung$status = (lung$status == 2) # convert to logical
survTask = makeSurvTask(data=lung, target=c("time","status"))
survTask

## cost-sensitive classification
df = iris
cost = matrix(runif(150 * 3, 0, 2000), 150) * (1 - diag(3))[df$Species,]
df$Species = NULL
costSensTask = makeCostSensTask(id="cost", data=df, cost=cost)
costSensTask

normalTask = makeClassifTask(id="normal", data=iris, target="Species")
normalTask

## see further arguments eg blocking, weights ...

## assessing the data set
str(getTaskData(classifTask))

## get the number of input variables
getTaskNFeats(classifTask)

## get input variable names
getTaskFeatureNames(classifTask)

## get values of target variable
head(getTaskTargets(classifTask))
head(getTaskTargets(survTask))

## get cost matrix in cost-sensitive classification
head(getTaskCosts(costSensTask))

## get task formula
getTaskFormula(classifTask)
getTaskFormulaAsString(survTask)

## deprecated function in the tutorial
# getTaskDescription
classifTask$task.desc

## getTaskTargetNames
classifTask$task.desc$target

## modifying a learning task
clusterTask
clusterTask = subsetTask(clusterTask, subset=4:17)
clusterTask

## remove constant features
removeConstantFeatures(clusterTask) # check 'perc' option

## remove selected features
dropFeatures(survTask, c("meal.cal", "wt.loss"))

## standardize numerical features
normalizeFeatures(classifTask, method="standardize")
# method - center, scale, standardize, range

# check more in data preprocessing