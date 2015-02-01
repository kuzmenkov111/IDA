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

save_plots=F

#install.packages("randomForest")

library(rpart)
library(randomForest)
library(car)

set.seed(0)

SLID = na.omit(SLID) # drop NA's


rf = randomForest( wages ~ ., data=SLID )
yhat = predict( rf, newdata=SLID )
print( sprintf( "MSE of the default fit %10.6f", mean( ( yhat - SLID$wages )^2 ) ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter5/prob_2_yhat_vs_y_default_params.eps", onefile=FALSE, horizontal=FALSE) }
plot( yhat, SLID$wages, xlab="predicted wages", ylab="wages (truth)", main="randomForest defaults" )
abline(a=0,b=1,col="green")
if( save_plots ){ dev.off() }


rf = randomForest( wages ~ ., data=SLID, mtry=4 )
yhat = predict( rf, newdata=SLID )
print( sprintf( "MSE with mtry=4 %10.6f", mean( ( yhat - SLID$wages )^2 ) ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter5/prob_2_yhat_vs_y_mtry_4.eps", onefile=FALSE, horizontal=FALSE) }
plot( yhat, SLID$wages, xlab="predicted wages", ylab="wages (truth)", main="randomForest mtry=4" )
abline(a=0,b=1,col="green")
if( save_plots ){ dev.off() }


rf = randomForest( wages ~ ., data=SLID, ntrees=100 )
yhat = predict( rf, newdata=SLID )
print( sprintf( "MSE with ntrees=100 %10.6f", mean( ( yhat - SLID$wages )^2 ) ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter5/prob_2_yhat_vs_y_ntrees_100.eps", onefile=FALSE, horizontal=FALSE) }
plot( yhat, SLID$wages, xlab="predicted wages", ylab="wages (truth)", main="randomForest ntrees=100" )
abline(a=0,b=1,col="green")
if( save_plots ){ dev.off() }


rf = randomForest( wages ~ ., data=SLID, ntrees=1000 )
yhat = predict( rf, newdata=SLID )
print( sprintf( "MSE with ntrees=1000 %10.6f", mean( ( yhat - SLID$wages )^2 ) ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter5/prob_2_yhat_vs_y_ntrees_1000.eps", onefile=FALSE, horizontal=FALSE) }
plot( yhat, SLID$wages, xlab="predicted wages", ylab="wages (truth)", main="randomForest ntrees=1000" )
abline(a=0,b=1,col="green")
if( save_plots ){ dev.off() }


