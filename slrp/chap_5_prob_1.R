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

#install.packages("randomForest")

library(rpart)
library(randomForest)

set.seed(0)

# Part (1):
#
x1 = rnorm(500)
x12 = x1^2
y = 1 + (-5*x12) + (5*rnorm(500))

x1_sorted = sort(x1)
y_truth = 1 - 5*(x1_sorted^2)


# Part (2) (fit a linear model of y to x12):
#
linear_model = lm( y ~ x12 )
summary( linear_model )

p_DF = data.frame( x12=(x1_sorted^2) )
yhat = predict( linear_model, newdata=p_DF )

postscript("../../WriteUp/Graphics/Chapter5/prob_1_f_X_from_x12.eps", onefile=FALSE, horizontal=FALSE)
plot( x1_sorted, y_truth, col="green", type="l", xlab="x1", ylab="y" )
lines( x1_sorted, yhat, type="p", col="red" )
dev.off()


# Part (3): Train a CART tree:
# 
tree = rpart( y ~ x1, method="anova" )

# View the CART tree:
# 
#postscript("../../WriteUp/Graphics/Chapter5/prob_1_tree_plot.eps", onefile=FALSE, horizontal=FALSE)
plot(tree)
text(tree,use.n=FALSE)
#dev.off()

# Lets get the CART predictions (in sample):
#
yhat_tree = predict( tree, data.frame( x1=x1_sorted ) )

#postscript("../../WriteUp/Graphics/Chapter5/prob_1_f_X_from_x1_CART.eps", onefile=FALSE, horizontal=FALSE)
plot( x1_sorted, y_truth, col="green", type="l", xlab="x1", ylab="y" )
lines( x1_sorted, yhat_tree, type="p", col="red" )
#dev.off()


# Part (4): Apply random forests to this problem: 
#
training_DF = data.frame( x1=x1, y=y ) # a data frame with a scalar input (x1) and a response
rf = randomForest( y ~ ., data=training_DF )
summary(rf)

# try to predict on some testing data: 
test_DF = data.frame( x1=x1_sorted )
yhat_rf = predict( rf, newdata=test_DF )

#postscript("../../WriteUp/Graphics/Chapter5/prob_1_f_X_from_x1_random_forest.eps", onefile=FALSE, horizontal=FALSE)
plot( x1_sorted, y_truth, col="green", type="l", xlab="x1", ylab="y" )
lines( x1_sorted, yhat_rf, type="p", col="red" )
#dev.off()


# Part (6): A partial dependence plot
#
#postscript("../../WriteUp/Graphics/Chapter5/prob_1_random_forest_partial_dependence.eps", onefile=FALSE, horizontal=FALSE)
partialPlot( rf, training_DF, x1 )
#dev.off()

# Variable importance can be assessed with :
importance( rf )

