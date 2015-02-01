#
# EPage 119
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

library(gam)
library(car)

m_glm = glm( lfp ~ age + inc + lwg, family=binomial, data=Mroz )
m_gam = gam( lfp ~ lo(age,degree=1) + lo(inc,degree=1) + lo(lwg,degree=1), family=binomial, data=Mroz )

s_glm = summary(m_glm)
s_gam = summary(m_gam)
print( sprintf("Null deviance= %10.6f", s_glm$null.deviance) )
print( sprintf("Residual deviance (GLM)= %10.6f", s_glm$deviance) )
print( sprintf("Residual deviance (GAM)= %10.6f", s_gam$deviance) )

#plot( m_glm )
