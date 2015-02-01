#
# EPage 184
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

library(DAAG)
library(gam)
library(rpart)
library(car)

set.seed(0)

Freedman = Freedman[ complete.cases(Freedman), ] 

# Part (1):
#
#postscript("../../WriteUp/Graphics/Chapter3/prob_3_pairs_plot.eps", onefile=FALSE, horizontal=FALSE)
pairs( Freedman, panel = panel.smooth, main = "Freedman" ) 
#dev.off()

# Perform the regression of crime rate on the three predictors (smoothed)
m1 = gam( crime ~ s(population,df=1) + s(nonwhite,df=1) + s(density,df=2), data=Freedman )
#m1 = gam( crime ~ lo(population,span=0.65) + lo(nonwhite,span=0.6) + lo(density,span=0.5), data=Freedman )

par(mfrow=c(1,3))
plot.gam(m1)
par(mfrow=c(1,1))

# Part (2) (fit a CART tree):
#
tree = rpart( crime ~ population + nonwhite + density, method="anova", data=Freedman )

print(tree)

#postscript("../../WriteUp/Graphics/Chapter3/prob_3_tree_plot.eps", onefile=FALSE, horizontal=FALSE)
plot(tree)
text(tree,use.n=F)
#dev.off()

# Part (3):
#
tree_predictions = predict( tree, data=Freedman )

#postscript("../../WriteUp/Graphics/Chapter3/prob_3_fitted_value_plot.eps", onefile=FALSE, horizontal=FALSE)

scatter.smooth( m1$fitted.values, tree_predictions, span=2/3, xlab="GAM fitted values", ylab="CART fitted values", main="model predictions of crime" )
fv_m = lm( tree_predictions ~ m1$fitted.values )
abline(fv_m, col="red")

#dev.off()

cor( m1$fitted.values, Freedman$crime )
cor( tree_predictions, Freedman$crime )
