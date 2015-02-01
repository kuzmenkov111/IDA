#
# EPage 342
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
library(MASS)

set.seed(0)

Pima.tr = na.omit( Pima.tr )

# Part (1): Train a svm on the input data frame:
#
m_svm = svm( type ~ ., data=Pima.tr )
y_hat = predict( m_svm, newdata=Pima.tr )
print( table( Pima.tr$type, y_hat ) )

m_svm = svm( type ~ ., data=Pima.tr, kernel="linear" )
y_hat = predict( m_svm, newdata=Pima.tr )
print( table( Pima.tr$type, y_hat ) )

# Part (2):
#
wts = table(Pima.tr$type)
print( wts / sum(wts) )
wts[1] = 0.34
wts[2] = 0.66

m_svm = svm( type ~ ., data=Pima.tr, class.weights=wts )
y_hat = predict( m_svm, newdata=Pima.tr )
print( table( Pima.tr$type, y_hat ) )

m_svm = svm( type ~ ., data=Pima.tr, class.weights=wts, kernel="linear" )
y_hat = predict( m_svm, newdata=Pima.tr )
print( table( Pima.tr$type, y_hat ) )

