#
# EPage 182
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

library(DAAG)
library(gam) # has smoothing splines
library(rpart)

set.seed(0)

data(frogs)

# Get just the data we desire:
# 
frogs = frogs[ , c(1,4,5,6,7,8,9,10) ]

# Part 1:
# 
linear_model = glm( pres.abs ~ ., family=binomial(), data=frogs )
summary( linear_model )

# Part 2:
# 
stepAIC( linear_model )

# Part 3 (create a confusion matrix) using the best model from stepAIC:
#
linear_model = glm( pres.abs ~ distance + NoOfPools + meanmin + meanmax, family=binomial(), data=frogs )

yes_frog_index = linear_model$fitted.values > 0.5
yhat = matrix( 0, nrow=length(linear_model$fitted.values), ncol=1 )
yhat[yes_frog_index] = 1

table( frogs$pres.abs, yhat ) # note the argument order is true class label followed by predicted class label

# Part 4 (gam smoothing)
#
m_gam = gam( pres.abs ~ s(distance) + s(NoOfPools) + s(meanmin) + s(meanmax), family=binomial(), data=frogs )

par(mfrow=c(2,2))
plot.gam(m_gam)
par(mfrow=c(1,1))

# Part 5 (in-sample classification performance of gam smoothing)
#
yes_frog_index = m_gam$fitted.values > 0.5
yhat = matrix( 0, nrow=length(linear_model$fitted.values), ncol=1 )
yhat[yes_frog_index] = 1

table( frogs$pres.abs, yhat ) 

# Part 6 (using rpart with all predictors)
#
tree = rpart( pres.abs ~ ., data=frogs, method="class" )

# View the CART tree:
# 
#postscript("../../WriteUp/Graphics/Chapter3/prob_2_tree_plot.eps", onefile=FALSE, horizontal=FALSE)
plot(tree)
text(tree,use.n=FALSE)
#dev.off()

# Part 7 (the performance of CART on the frogs dataset)
#
yhat = matrix( 0, nrow=dim(frogs)[1], ncol=1 )
phat = predict( tree, frogs )
yhat[ phat[,2] > 0.5 ] = 1
L_cart_default = table( frogs$pres.abs, yhat )

# Part 8 (different priors for CART and their performance)
#
tree_8_50p = rpart( pres.abs ~ ., parms=list(prior=c(0.5,0.5)), data=frogs, method="class" )
yhat = matrix( 0, nrow=dim(frogs)[1], ncol=1 )
phat = predict( tree_8_50p, frogs )
yhat[ phat[,2] > 0.5 ] = 1
L_cart_50p = table( frogs$pres.abs, yhat )
print("CART with 50-50 prior split")
print( L_cart_50p )

tree_8_30p = rpart( pres.abs ~ ., parms=list(prior=c(0.7,0.3)), data=frogs, method="class" )
yhat = matrix( 0, nrow=dim(frogs)[1], ncol=1 )
phat = predict( tree_8_30p, frogs )
yhat[ phat[,2] > 0.5 ] = 1
L_cart_30p = table( frogs$pres.abs, yhat )
print("CART with 70-30 prior split")
print( L_cart_30p )

print( sprintf("Default CART classificaion error %10.6f; FN/FP = %10.6f", 1 - ( L_cart_default[1,1] + L_cart_default[2,2] ) /sum(L_cart_default), L_cart_default[2,1]/L_cart_default[1,2] ) )
print( sprintf("CART with 50-50 split classificaion error %10.6f; FN/FP = %10.6f", 1 - ( L_cart_50p[1,1] + L_cart_50p[2,2] ) /sum(L_cart_50p), L_cart_50p[2,1]/L_cart_50p[1,2] ) )
print( sprintf("CART with 70-30 split classificaion error %10.6f; FN/FP = %10.6f", 1 - ( L_cart_30p[1,1] + L_cart_30p[2,2] ) /sum(L_cart_30p), L_cart_30p[2,1]/L_cart_30p[1,2] ) )

# Part 9 (skewing the false positive and false negative ratio with prioris):
#
# Compute the emprirical priors:
# 
pi_0 = sum(frogs$pres.abs==0) / (sum(frogs$pres.abs==0) + sum(frogs$pres.abs==1) )
pi_1 = 1 - pi_0 

# Compute adjusted priors (set to skew the false negative / false positive ratio of the resulting classifier):
#
L_0 = 10 # a false positive (L_0) is 10 times the cost of a false negative (L_1)
L_1 = 1
L_0 = 1  # a false negative (L_1) is 10 times more costly than a false positive (L_0)
L_1 = 10
pi_0_star = ( pi_0 * L_0 ) / ( pi_0 * L_0 + pi_1 * L_1 ) 
pi_1_star = ( pi_1 * L_1 ) / ( pi_0 * L_0 + pi_1 * L_1 )

tree_8_lean_priors = rpart( pres.abs ~ ., parms=list(prior=c(pi_0_star,1-pi_0_star)), data=frogs, method="class" )
yhat = matrix( 0, nrow=dim(frogs)[1], ncol=1 )
phat = predict( tree_8_lean_priors, frogs )
yhat[ phat[,2] > 0.5 ] = 1
L_cart_lean_priors = table( frogs$pres.abs, yhat )
print( sprintf( "CART with %5.2f-%5.2f prior split", pi_0_star, pi_1_star ) )
print( L_cart_lean_priors )

#postscript("../../WriteUp/Graphics/Chapter3/prob_2_skewed_tree_plot.eps", onefile=FALSE, horizontal=FALSE)
plot(tree_8_lean_priors)
text(tree_8_lean_priors,use.n=FALSE)
#dev.off()



