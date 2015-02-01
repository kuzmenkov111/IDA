#
# EPage 271
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

save_plots=T

#install.packages("randomForest")

library(rpart)
library(randomForest)
library(MASS)

set.seed(0)

Pima.tr = na.omit(Pima.tr) # drop NA's

# Part 1:
# 
rf = randomForest( type ~ ., data=Pima.tr )
yhat = predict( rf, newdata=Pima.tr )

T = table( Pima.tr$type, rf$predicted )
print( sprintf( "Accuracy (overall) = %10.6f", (T[1,1]+T[2,2])/sum(T) ) )
print( sprintf( "Accuracy when truth is No= %10.6f", (T[1,1])/(T[1,1]+T[1,2]) ) )
print( sprintf( "Accuracy when truth is Yes= %10.6f", (T[2,2])/(T[2,1]+T[2,2]) ) )
print( sprintf( "Accuracy when predicting No= %10.6f", (T[1,1])/(T[1,1]+T[2,1]) ) )
print( sprintf( "Accuracy when predicting Yes= %10.6f", (T[2,2])/(T[1,2]+T[2,2]) ) )
