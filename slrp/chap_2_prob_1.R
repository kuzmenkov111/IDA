#
# EPage 118
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

# Drop any NA's:
#
airquality = airquality[ complete.cases(airquality), ]

# Reorder the data frame so that Temp is increasing as a function of rows:
#
airquality = airquality[ with(airquality, order(Temp)), ] 

Temp = airquality$Temp
Ozone = airquality$Ozone

m1 = gam( Ozone ~ Temp ) # red
m2 = gam( Ozone ~ as.factor(Temp) )# blue
m3 = gam( Ozone ~ s(Temp) ) # green

#postscript("../../WriteUp/Graphics/Chapter2/prob_1_gam_plot.eps", onefile=FALSE, horizontal=FALSE)
#
plot( Temp, Ozone, col="black" )
lines( Temp, predict( m1 ), col="red" )
lines( Temp, predict( m2 ), col="blue" )
lines( Temp, predict( m3 ), col="green" )
#
#dev.off()

# Extract the residual deviance:
#
print( c( summary(m1)$deviance, summary(m2)$deviance, summary(m3)$deviance ) )

# Extract the AIC for the three models:
#
print( c( summary(m1)$aic, summary(m2)$aic, summary(m3)$aic ) )

# Part (2) various loess smooths:
#
#postscript("../../WriteUp/Graphics/Chapter2/prob_1_loess_plot.eps", onefile=FALSE, horizontal=FALSE)
#
par(mfrow=c(3,3))
for( s in c(0.25,0.5,0.75) ){
  for( d in c(0,1,2) ){
    plot( Temp, Ozone, col='darkgray', main=sprintf("span=%1.2f; deg=%1d", s, d) )
    ls = loess.smooth( Temp, Ozone, span=s, degree=d ) # extracts the loess fitted values 
    lines( ls$x, ls$y )
  }
}
par(mfrow=c(1,1))
#
#dev.off()

# Part (5) simpleboot ... I couln't get this to work with my version of R or the version of this package
#
#install.packages("simpleboot")
if( FALSE ){
  library(simpleboot)

  smooth = loess( Ozone ~ Temp, span=0.5 )
  bo = loess.boot( smooth, R=10, rows=FALSE, ngrid=20 )
  plot(bo)
}





