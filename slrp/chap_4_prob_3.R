#
# EPage 209
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
library(DAAG)

set.seed(0)

# Part (1):
#
frogs$pres.abs = as.factor( frogs$pres.abs )
tree = rpart( pres.abs ~ ., method="class", data=frogs )
p_hat = predict( tree, newdata=frogs )
y_hat_CART = matrix( 0, ncol=dim(frogs)[1], nrow=1 )
y_hat_CART[ p_hat[,1] > 0.5 ] = 1
table( frogs$pres.abs, y_hat_CART ) 

bag = bagging( pres.abs ~ ., data=frogs, nbagg=25, coob=TRUE )
y_hat_BAGGING = predict( bag, newdata=frogs )
table( frogs$pres.abs, y_hat_BAGGING )

# Part (2):
# 
table( y_hat_CART, y_hat_BAGGING )

