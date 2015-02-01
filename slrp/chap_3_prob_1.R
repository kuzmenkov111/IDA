#
# EPage 181
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

library(rpart)

set.seed(0)


# Part (1):
#
x1 = rnorm(300)
x2 = rnorm(300)
error = 2 * rnorm(300)
y1 = 1 + (2*x1) + (3*x2) + error 

# Print the summary stats from the linear fit:
#
linear_model = lm( y1 ~ x1 + x2 )
summary( linear_model )

# Train a CART regression tree:
# 
tree = rpart( y1 ~ x1 + x2, method="anova" )

# View the CART tree:
# 
#postscript("../../WriteUp/Graphics/Chapter3/prob_1_tree_plot.eps", onefile=FALSE, horizontal=FALSE)
plot(tree)
text(tree,use.n=FALSE)
#dev.off()

# Lets get the CART and lm predictions (in sample):
#
yhat_lm = linear_model$fitted.values
yhat_tree = predict( tree, data.frame( x1, x2 ) )
print( sprintf("IS: linear model MSE= %10.6f", mean( (yhat_lm - y1)^2 ) ) )
print( sprintf("IS: regression tree MSE= %10.6f", mean( (yhat_tree - y1)^2 ) ) )

# Lets get the CART and lm predictions (out-of-sample) by generating more data:
#
x1_oos = rnorm(300)
x2_oos = rnorm(300)
error_oos = 2 * rnorm(300)
y1_oos = 1 + (2*x1_oos) + (3*x2_oos) + error_oos

DF_oos = data.frame( x1_oos, x2_oos ); colnames(DF_oos)=c("x1","x2")
yhat_lm = predict( linear_model, DF_oos )
yhat_tree = predict( tree, DF_oos )

print( sprintf("OOS: linear model MSE= %10.6f", mean( (yhat_lm - y1_oos)^2 ) ) )
print( sprintf("OOS: regression tree MSE= %10.6f", mean( (yhat_tree - y1_oos)^2 ) ) )


# Part (2):
#
x11 = (x1 > 0)
x22 = (x2 > 0)
y = 1 + (2*x11) + (3*x22) + error

# Print the summary stats from the linear fit:
#
linear_model = lm( y ~ x11 + x22 )
summary( linear_model )

# Train the CART tree:
#
tree = rpart( y ~ x11 + x22, method="anova" )

# View the CART tree:
#
#postscript("../../WriteUp/Graphics/Chapter3/prob_1_factor_tree_plot.eps", onefile=FALSE, horizontal=FALSE)
plot(tree)
text(tree,use.n=FALSE)
#dev.off()

# Lets get the CART and lm predictions (in sample):
#
yhat_lm = linear_model$fitted.values
yhat_tree = predict( tree, data.frame( x11, x22 ) )
print( sprintf("IS: linear model MSE= %10.6f", mean( (yhat_lm - y)^2 ) ) )
print( sprintf("IS: regression tree MSE= %10.6f", mean( (yhat_tree - y)^2 ) ) )

# Lets get the CART and lm predictions (out-of-sample) by generating more data:
#
x11_oos = (rnorm(300)>0)
x22_oos = (rnorm(300)>0)
error_oos = 2 * rnorm(300)
y_oos = 1 + (2*x11_oos) + (3*x22_oos) + error_oos

DF_oos = data.frame( x11_oos, x22_oos ); colnames(DF_oos)=c("x1","x2")
yhat_lm = predict( linear_model, DF_oos )
yhat_tree = predict( tree, DF_oos )

print( sprintf("OOS: linear model MSE= %10.6f", mean( (yhat_lm - y_oos)^2 ) ) )
print( sprintf("OOS: regression tree MSE= %10.6f", mean( (yhat_tree - y_oos)^2 ) ) )

