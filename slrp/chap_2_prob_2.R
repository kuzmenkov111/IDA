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

#install.packages("assist")
library(assist)

data(TXtemp)

# Part (1):
# 
m0 = gam( mmtemp ~ lat + long, data=TXtemp ) # fit a model to the data "as is"
m1 = gam( mmtemp ~ lo(lat,degree=1) + lo(long,degree=1), data=TXtemp ) # fit a model on the loess smoothed predictors

s0 = summary(m0)
s1 = summary(m1)
print( sprintf("Null deviance= %10.6f", s0$null.deviance) )
print( sprintf("Residual deviance (m0)= %10.6f", s0$deviance) )
print( sprintf("Residual deviance (m1)= %10.6f", s1$deviance) )

#postscript("../../WriteUp/Graphics/Chapter2/prob_2_gam_plot.eps", onefile=FALSE, horizontal=FALSE)
#
par(mfrow=c(1,2))
plot.gam(m1, residuals=F, se=TRUE)
par(mfrow=c(1,1))
#
#dev.off()

# Part (2):
#
m2 = gam( mmtemp ~ lo(lat + long,degree=1), data=TXtemp ) # fit a model on 2D loess smoothed predictors

s2 = summary(m2)
print( sprintf("Residual deviance (m2)= %10.6f", s2$deviance) )

plot.gam(m2)

