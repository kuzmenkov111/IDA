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
trainData.cl = data.cl[trainIndex,]
testData.cl = data.cl[-trainIndex,]

## instantiate rpartDT
set.seed(12357)
cl = cartDT(trainData.cl, testData.cl, "High ~ .", ntree=1000)

# class and names
class(cl)
names(cl)

## cp values
# cart
round(t(data.frame(c(cl$rpt$cp[1,][2],cl$rpt$cp[1,][1]))),4)
# bagging
round(cl$boot.cp[,1:5],4)

## errors
crt.err = data.frame(train.lst.err=cl$rpt$train.lst$error$error
                     ,test.lst.err=cl$rpt$test.lst$error$error)

## individual errors
## single tree's train and test errors are quite lower
## for oob error, 3 sd away / for test error, 1.4 sd away
## if the mean of oob errors approximately equals to poterior means, the train error rate of the single tree may be unreasonable
(mean(unlist(cl$ind.oob.lst.err)) - crt.err[[1]])/sd(unlist(cl$ind.oob.lst.err))
(mean(unlist(cl$ind.tst.lst.err)) - crt.err[[2]])/sd(unlist(cl$ind.tst.lst.err))

# cart train error
crt.err[1]

# oob error at least xerror - se to see 1-SE rule
summary(unlist(cl$ind.oob.lst.err))

ind.oob.error = data.frame(error=unlist(cl$ind.oob.lst.err))
ggplot(ind.oob.error, aes(x=error)) + 
  geom_histogram() + geom_vline(xintercept=crt.err[[1]], color="blue")

# cart test error
crt.err[2]

# individual test error
summary(unlist(cl$ind.tst.lst.err))

ind.tst.error = data.frame(error=unlist(cl$ind.tst.lst.err))
ggplot(ind.tst.error, aes(x=error)) + 
  geom_histogram() + geom_vline(xintercept=crt.err[[2]], color="blue")

## cumulative errors
## for oob error, the bagging error stays around 0.225, which is well over the training error of 0.16
## for test error, the bagging error converges to around 0.164, which is lower than 0.19
## bagging doesn't overestimate and provided better prediction
# oob error
summary(unlist(cl$cum.oob.lst.err))

bgg.oob.err = data.frame(sample=1:length(cl$cum.oob.lst.err)
                         ,error=unlist(cl$cum.oob.lst.err))
ggplot(data=bgg.oob.err,aes(x=sample,y=error)) + 
  geom_line() + ylim(crt.err[[1]],max(bgg.oob.err$error)) + 
  geom_abline(intercept=crt.err[[1]],slope=0,color="blue")

# test error
summary(unlist(cl$cum.tst.lst.err))

bgg.tst.err = data.frame(sample=1:length(cl$cum.tst.lst.err)
                         ,error=unlist(cl$cum.tst.lst.err))
ggplot(data=bgg.tst.err,aes(x=sample,y=error)) + 
  geom_line() + geom_abline(intercept=crt.err[[2]],slope=0,color="blue")

## variable importance
## variable importance is somewhat different - US, Urban and Education are rarely considered in the single tree
# cart
cart.varImp = data.frame(method="cart"
                         ,variable=names(cl$rpt$mod$variable.importance)
                         ,value=cl$rpt$mod$variable.importance/sum(cl$rpt$mod$variable.importance)
                         ,row.names=NULL)
# bagging - lst to se to see 1-SE rule
ntree = 1000
bgg.varImp = data.frame(method="bagging"
                        ,variable=rownames(cl$cum.varImp.lst)
                        ,value=cl$cum.varImp.lst[,ntree])
# plot variable importance measure
cl.varImp = rbind(cart.varImp,bgg.varImp)
cl.varImp$variable = reorder(cl.varImp$variable, 1/cl.varImp$value)
ggplot(data=cl.varImp,aes(x=variable,y=value,fill=method)) + geom_bar(stat="identity")



