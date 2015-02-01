#
# EPage 65
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

library(MASS)

# Generate data:
# 
data = rnorm(50*100)
M = matrix( data, nrow=50, ncol=100 )
DF = data.frame(M) 

# Fit a linear model:
# 
m = lm( DF )

# Apply stepwise regression on this random noise data:
#
sm = stepAIC( m ) 

