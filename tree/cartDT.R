## error
# oob error
# test error
## individual error
# non-parametric bootstrap mean approximately equals to non-informative posterior distribution
# CI-like interval may be guessed from the distribution of individual trees
## cumulative error
# relieving concern of over-estimation by averaging or majority voting
# more reliable prediction
## variable importance
# if single tree's variable importance is far different from that of bagged trees
# can check if there are dominant predictors - main benefit of bagging is through reducing variance

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
rg = cartDT(trainData.rg, testData.rg, "Sales ~ .", ntree=10)

# class and names
class(rg)
# rpt - single tree, lst - least xerror, se - 1-SE rule, oob - out-of-bag sample, tst - test data, ind (cum) - individual (cumulative) prediction or error
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
# cart train error
crt.err[1]

# oob error at least xerror - se to see 1-SE rule
# only 10 trees, 0 is not included but not a big deal
summary(unlist(rg$ind.oob.lst.err))

# cart test error
crt.err[2]

# individual test error
summary(unlist(rg$ind.tst.lst.err))

## cumulative errors
# oob error
summary(unlist(rg$cum.oob.lst.err))

# test error
summary(unlist(rg$cum.tst.lst.err))

## variable importance
cart.varImp = data.frame(method="cart"
                         ,variable=names(rg$rpt$mod$variable.importance)
                         ,value=rg$rpt$mod$variable.importance/sum(rg$rpt$mod$variable.importance)
                         ,row.names=NULL)
# bagging - lst to se to see 1-SE rule
bgg.varImp = data.frame(method="bagging"
                        ,variable=rownames(rg$cum.varImp.lst)
                        ,value=rg$cum.varImp.lst[,10])
rg.varImp = rbind(cart.varImp,bgg.varImp)
rg.varImp