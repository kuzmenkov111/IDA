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

set.seed(0)

# Part (1):
#
x1 = rnorm(500)
x12 = x1^2
y = 1 + (2*x12) + (2*rnorm(500))

x1_sorted = sort(x1)
y_truth = 1 + 2*(x1_sorted^2)


# Part (2) (fit a linear model of y to x12):
#
linear_model = lm( y ~ x12 )
summary( linear_model )

p_DF = data.frame( x12=(x1_sorted^2) )
yhat = predict( linear_model, newdata=p_DF )

#postscript("../../WriteUp/Graphics/Chapter4/prob_1_f_X_from_x12.eps", onefile=FALSE, horizontal=FALSE)
plot( x1_sorted, y_truth, col="green", type="l", xlab="x1", ylab="y" )
lines( x1_sorted, yhat, type="p", col="red" )
#dev.off()


# Part (3) (fit a linear model of y to x1):
#
linear_model = lm( y ~ x1 )
summary( linear_model )

p_DF = data.frame( x1=x1_sorted )
yhat = predict( linear_model, newdata=p_DF )

#postscript("../../WriteUp/Graphics/Chapter4/prob_1_f_X_from_x1.eps", onefile=FALSE, horizontal=FALSE)
plot( x1_sorted, y_truth, col="green", type="l", xlab="x1", ylab="y" )
lines( x1_sorted, yhat, type="p", col="red" )
#dev.off()


# Part (4): Train a CART tree:
# 
tree = rpart( y ~ x1, method="anova" )

# View the CART tree:
# 
#postscript("../../WriteUp/Graphics/Chapter4/prob_1_tree_plot.eps", onefile=FALSE, horizontal=FALSE)
plot(tree)
text(tree,use.n=FALSE)
#dev.off()

# Lets get the CART predictions (in sample):
#
yhat_tree = predict( tree, data.frame( x1=x1_sorted ) )

#postscript("../../WriteUp/Graphics/Chapter4/prob_1_f_X_from_x1_CART.eps", onefile=FALSE, horizontal=FALSE)
plot( x1_sorted, y_truth, col="green", type="l", xlab="x1", ylab="y" )
lines( x1_sorted, yhat_tree, type="p", col="red" )
#dev.off()


# Part (5): Apply bagging to this problem: 
#

# NOTE: I don't know why this section of code does not work correctly ... seems like there maybe a bug in the implementation.
# If any one knows why this does not work please let me know 
#
if( FALSE ){ 
    training_DF = data.frame( x1=x1, y=y ) # a data frame with a scalar input (x1) and a response
    bag = bagging( y ~ ., data=training_DF, nbagg=25 )
    summary(bag) # Why does this output give Length for X of 500?  Shouldn't it be 1?

    # try to predict on some testing data: 
    test_DF = data.frame( x1=x1_sorted )
    yhat_bag = predict( bag, newdata=test_DF )

    # Why does the output to predict seem to be scrambled i.e. not in the order x1_sorted?
    plot( x1_sorted, y_truth, col="green", type="l", xlab="x1", ylab="y" )
    lines( x1_sorted, yhat_bag, type="p", col="red" )
}

# When I add a dummy variable (making this problem two dimensional) things seem to work as expected: 
#
training_DF = data.frame( x1=x1, x2=rep(1,length(x1)), y=y ) # Note that x2 is a dummy variable ... has no predictive power 
bag = bagging( y ~ ., data=training_DF, nbagg=25 ) # bag is of class regbagg (regression bagging)
summary(bag) # This gives Length for X of 2 ... which seems correct matches other experience.

# try to predict on some testing data: 
test_DF = data.frame( x1=x1_sorted, x2=rep(1,length(x1)) )
yhat_bag = predict( bag, newdata=test_DF )

# This output looks like its doing what one would expect 
#postscript("../../WriteUp/Graphics/Chapter4/prob_1_f_X_from_x1_CART_bagging.eps", onefile=FALSE, horizontal=FALSE)
plot( x1_sorted, y_truth, col="green", type="l", xlab="x1", ylab="y" )
lines( x1_sorted, yhat_bag, type="p", col="red" )
#dev.off()

