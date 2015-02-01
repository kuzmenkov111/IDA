#
# EPage 313
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

save_plots = F

library(car)
library(gbm)

set.seed(0)

# Part (1): Train a gbm: 
#
Leinhardt = na.omit( Leinhardt )
boosts = gbm( infant ~ ., distribution="gaussian", n.trees=15000, data=Leinhardt, cv.folds=5 )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_2_gbm_perf.eps", onefile=FALSE, horizontal=FALSE) }
opt_ntrees = gbm.perf( boosts, method="cv" )
if( save_plots ){ dev.off() }

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_2_rel_influence.eps", onefile=FALSE, horizontal=FALSE) }
summary(boosts, n.trees=opt_ntrees)
if( save_plots ){ dev.off() }

# Compute the marginal effects of the given variables by integrating out the other variables (i.e. the one dimensional dependence)
#
if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_2_pd_region.eps", onefile=FALSE, horizontal=FALSE) }
plot( boosts, i.var="region", n.trees=opt_ntrees )
if( save_plots ){ dev.off() }

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_2_pd_income.eps", onefile=FALSE, horizontal=FALSE) }
plot( boosts, i.var="income", n.trees=opt_ntrees )
if( save_plots ){ dev.off() }

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_2_pd_oil.eps", onefile=FALSE, horizontal=FALSE) }
plot( boosts, i.var="oil", n.trees=opt_ntrees )
if( save_plots ){ dev.off() }

# Two / Three variable partial dependence plots:
#
if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_2_pd_region_income.eps", onefile=FALSE, horizontal=FALSE) }
plot( boosts, i.var=c("region","income"), n.trees=opt_ntrees )
if( save_plots ){ dev.off() }

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_2_pd_region_income_oil.eps", onefile=FALSE, horizontal=FALSE) }
plot( boosts, i.var=c("region","income","oil"), n.trees=opt_ntrees )
if( save_plots ){ dev.off() }

# Part (9): Try to use a random forest on this data and see if we get the same results: 
#
library(rpart)
library(randomForest)

rf = randomForest(  infant ~ ., data=Leinhardt )

importance( rf ) # variable importance measure 

partialPlot( rf, pred.data=Leinhardt, x.var="region" ) # partial dependence plots
partialPlot( rf, pred.data=Leinhardt, x.var="income" )
partialPlot( rf, pred.data=Leinhardt, x.var="oil" )
