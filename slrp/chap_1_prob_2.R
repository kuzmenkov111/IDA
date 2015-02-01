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

# Part (1):
# 
x = rep(1:20, times=10)
y = rnorm(200)

#postscript("../../WriteUp/Graphics/Chapter1/prob_2_scatter_plot.eps", onefile=FALSE, horizontal=FALSE)
plot( x, y )
#dev.off()

summary( lm( y ~ x ) )
summary( glm( y ~ x ) )

summary( lm( y ~ as.factor(x) ) )
summary( glm( y ~ as.factor(x) ) )


