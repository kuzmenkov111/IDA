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

# install.packages("Rlab")
# install.packages("e1071")

library(Rlab)
library(e1071)

set.seed(0)

# Part (1):
#
n_pts = 500
w = rnorm(n_pts)
z = rnorm(n_pts)
w2 = w^2
x = (-1 + 3*w2 - z)
p = exp(x) / ( 1 + exp(x) )
y = rbern(n_pts,p)
y_factor = as.factor(y)

# Part (1): This is an incorrect model:
# 
m_glm = glm( y_factor ~ w + z, family=binomial )
print( summary(m_glm) )
p_hat = predict( m_glm, newdata=data.frame( w=w, z=z ) )
y_hat = as.double( p_hat > 0.5 )
print( table( y, y_hat ) )

# Part (1): This is the correct model:
#
m_glm = glm( y_factor ~ w2 + z, family=binomial )
print( summary(m_glm) )
p_hat = predict( m_glm, newdata=data.frame( w2=w2, z=z ) )
y_hat = as.double( p_hat > 0.5 )
print( table( y, y_hat ) )


# Part (2): Train a svm on the two sets of variables:
#
m_svm = svm( y_factor ~ w + z )
y_hat = predict( m_svm, newdata=data.frame( w=w, z=z ) )
print( table( y, y_hat ) )

m_svm = svm( y_factor ~ w2 + z )
y_hat = predict( m_svm, newdata=data.frame( w2=w2, z=z ) )
print( table( y, y_hat ) )

# Try a linear kernel and show that the performance IS different (as in the first examples)
m_svm = svm( y_factor ~ w + z, kernel="linear" )
y_hat = predict( m_svm, newdata=data.frame( w=w, z=z ) )
print( table( y, y_hat ) )

m_svm = svm( y_factor ~ w2 + z, kernel="linear" )
y_hat = predict( m_svm, newdata=data.frame( w2=w2, z=z ) )
print( table( y, y_hat ) )
