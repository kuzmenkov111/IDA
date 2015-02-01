#
# EPage 208
#
# Written by:
# -- 
# John L. Weatherwax                2009-04-21
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

#install.packages("ipred")
#install.packages("mlbench")

library(rpart)
library(ipred)
library(car)

set.seed(0)

# Part (1):
#
tree = rpart( crime ~ ., method="anova", data=Freedman )
yhat = predict( tree, newdata=Freedman )

rms = sqrt( mean( ( yhat - Freedman$crime )^2 ) )
print( sprintf( "CART RMS= %10.6f", rms ) )
print( sprintf( "CART sd(yhat)= %10.6f", sd(yhat) ) )

bag = bagging( crime ~ ., data=Freedman, nbagg=25, coob=TRUE )
yhat = predict( bag, newdata=Freedman )
print(bag)
print( sprintf( "BAGGING sd(yhat)= %10.6f", sd(yhat) ) )

