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

library(gbm)

set.seed(0)

# Part (1):
#
x1 = rnorm(1000)
x12 = x1^2
ysys = 1 + (-5*x12)
y = ysys + (5*rnorm(500))
dta = data.frame(y,x1,x12)

x1_sorted = sort(x1)
y_truth = 1 - 5*(x1_sorted^2)

max_x = max(x1_sorted)
min_x = min(x1_sorted)
max_y = max(y_truth)
min_y = min(y_truth)

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_1_f_of_X.eps", onefile=FALSE, horizontal=FALSE) }
plot( x1_sorted, y_truth, col="green", type="l", xlab="x1", ylab="y" )
if( save_plots ){ dev.off() }

# Part (2): Train a gbm: 
#
dta1 = data.frame(y,x1)
boosts = gbm( y ~ x1, distribution="gaussian", n.trees=50000, data=dta1, cv.folds=5 )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_1_gbm_perf.eps", onefile=FALSE, horizontal=FALSE) }
opt_ntrees = gbm.perf( boosts, method="cv" )
if( save_plots ){ dev.off() }

# View the partial dependence plot for various number of trees: 
#
n_trees = c(100,500,1000,5000,10000,opt_ntrees)
for( nt in n_trees ){
  if( save_plots ){
    fn = sprintf( "../../WriteUp/Graphics/Chapter6/prob_1_gbm_partial_dependence_w_ntrees_%d.eps", nt )
    postscript(fn, onefile=FALSE, horizontal=FALSE)
  }
  plot( boosts, n.trees=nt, xlim=c(max_x,min_x), ylim=c(min_y,max_y) )
  lines( x1_sorted, y_truth, type='l', col='green' )
  if( save_plots ){ dev.off() }
}



