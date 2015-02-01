library(dplyr)
library(caret)

### data
require(ISLR)
data(Carseats)
Carseats = Carseats %>% 
  mutate(High=factor(ifelse(Sales<=8,"No","High"),labels=c("High","No")))
# structure of predictors
str(subset(Carseats,select=c(-High,-Sales)))
# classification response summary
with(Carseats,table(High))
with(Carseats,table(High) / length(High))

# regression response summary
summary(Carseats$Sales)

### Classification
## Data Splitting
set.seed(1237)
trainIndex.cl = createDataPartition(Carseats$High, p=.8, list=FALSE, times=1)
trainData.cl = subset(Carseats, select=c(-Sales))[trainIndex.cl,]
testData.cl = subset(Carseats, select=c(-Sales))[-trainIndex.cl,]

# training response summary
with(trainData.cl,table(High))
with(trainData.cl,table(High) / length(High))

# test response summary
with(testData.cl,table(High))
with(testData.cl,table(High) / length(High))

## Model Training and Tuning
trControl.cv = trainControl(method="cv",number=10)
trControl.recv = trainControl(method="repeatedcv",number=10,repeats=5)
trControl.boot = trainControl(method="boot",number=50)

set.seed(12357)
fit.cl.cv = train(High ~ ., data=trainData.cl, method="rpart"
                  ,tuneLength=20, trControl=trControl.cv)
subset(fit.cl.cv$results,subset=cp==fit.cl.cv$bestTune$cp)

fit.cl.recv = train(High ~ ., data=trainData.cl, method="rpart"
                  ,tuneLength=20, trControl=trControl.recv)
subset(fit.cl.recv$results,subset=cp==fit.cl.recv$bestTune$cp)

fit.cl.boot = train(High ~ ., data=trainData.cl, method="rpart"
                  ,tuneLength=20, trControl=trControl.boot)
subset(fit.cl.boot$results,subset=cp==fit.cl.boot$bestTune$cp)

# select the best cp value from repeated cv strategy
cp.cl = fit.cl.recv$bestTune$cp

### Regression












