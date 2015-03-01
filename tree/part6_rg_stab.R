## data
require(ISLR)
data(Carseats)
require(dplyr)
Carseats = Carseats %>% 
  mutate(High=factor(ifelse(Sales<=8,"No","High"),labels=c("High","No")))
data.cl = subset(Carseats, select=c(-Sales))
data.rg = subset(Carseats, select=c(-High))

## import constructors
source("src/cart.R")

# split - cl: classification, rg: regression
require(caret)
set.seed(1237)
trainIndex = createDataPartition(Carseats$High, p=0.8, list=FALSE, times=1)
trainData.rg = data.rg[trainIndex,]
testData.rg = data.rg[-trainIndex,]

## instantiate rpartDT
set.seed(12357)
rg = cartDT(trainData.rg, testData.rg, "Sales ~ .", ntree=2000)

# class and names
class(rg)
names(rg)

## cp values
# cart
round(t(data.frame(c(rg$rpt$cp[1,][2],rg$rpt$cp[1,][1]))),4)
# bagging
round(rg$boot.cp[,1:5],4)

## errors
crt.err = data.frame(train.lst.err=rg$rpt$train.lst$error$error
                     ,test.lst.err=rg$rpt$test.lst$error$error)

## individual errors
## single tree's train and test errors are not far away from the corresponding errors of the bagged trees
## oob error 1.3 and test error 0.65 sd
(mean(unlist(rg$ind.oob.lst.err)) - crt.err[[1]])/sd(unlist(rg$ind.oob.lst.err))
(mean(unlist(rg$ind.tst.lst.err)) - crt.err[[2]])/sd(unlist(rg$ind.tst.lst.err))

# cart train error
crt.err[1]

# oob error at least xerror - se to see 1-SE rule
summary(unlist(rg$ind.oob.lst.err))

ind.oob.error = data.frame(error=unlist(rg$ind.oob.lst.err))
ggplot(ind.oob.error, aes(x=error)) + 
  geom_histogram() + geom_vline(xintercept=crt.err[[1]], color="blue")

# cart test error
crt.err[2]

# individual test error
summary(unlist(rg$ind.tst.lst.err))

ind.tst.error = data.frame(error=unlist(rg$ind.tst.lst.err))
ggplot(ind.tst.error, aes(x=error)) + 
  geom_histogram() + geom_vline(xintercept=crt.err[[2]], color="blue")

## cumulative errors
## single tree's train error is 0 and, around 1000 trees, the oob error stays similar 0.005 at 2000th tree
## roughly the test errors of the bagged tree is around 0.5, which is well lower than the test error of the single tree (0.74)
## bagging doesn't overestimate and provided better prediction
# oob error
summary(unlist(rg$cum.oob.lst.err))

bgg.oob.err = data.frame(sample=1:length(rg$cum.oob.lst.err)
                         ,error=unlist(rg$cum.oob.lst.err))
ggplot(data=bgg.oob.err,aes(x=sample,y=error)) + 
  geom_line() + geom_abline(intercept=crt.err[[1]],slope=0,color="blue")

# test error
summary(unlist(rg$cum.tst.lst.err))

bgg.tst.err = data.frame(sample=1:length(rg$cum.tst.lst.err)
                         ,error=unlist(rg$cum.tst.lst.err))
ggplot(data=bgg.tst.err,aes(x=sample,y=error)) + 
  geom_line() + geom_abline(intercept=crt.err[[2]],slope=0,color="blue")
  
## variable importance
## similar variable importance between the bagged and single tree
## ShelveLoc and Price has the two most importance, the bagged trees may be dependent on them than other methods
# cart
cart.varImp = data.frame(method="cart"
                         ,variable=names(rg$rpt$mod$variable.importance)
                         ,value=rg$rpt$mod$variable.importance/sum(rg$rpt$mod$variable.importance)
                         ,row.names=NULL)
# bagging - lst to se to see 1-SE rule
ntree = 1000
bgg.varImp = data.frame(method="bagging"
                        ,variable=rownames(rg$cum.varImp.lst)
                        ,value=rg$cum.varImp.lst[,ntree])
# plot variable importance measure
rg.varImp = rbind(cart.varImp,bgg.varImp)
rg.varImp$variable = reorder(rg.varImp$variable, 1/rg.varImp$value)
ggplot(data=rg.varImp,aes(x=variable,y=value,fill=method)) + geom_bar(stat="identity")